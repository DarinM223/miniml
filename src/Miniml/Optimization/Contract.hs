{-# LANGUAGE OverloadedLabels #-}

module Miniml.Optimization.Contract where

import Control.Monad.State.Strict (State, execState, gets, modify')
import Data.Foldable (for_, traverse_)
import Data.IntMap qualified as IM
import GHC.Generics (Generic)
import Miniml.Cps (Cexp (..), Value (Label, Var), Var)
import Miniml.Shared (Access, Primop)
import Optics (at', (%), _Just)
import Optics.State.Operators ((%=))

type ContractInfo = IM.IntMap Info

data Info = Info
  { specific :: {-# UNPACK #-} !SpecificInfo,
    -- | number of times a variable has been referred to
    used :: {-# UNPACK #-} !Int,
    -- | number of times a variable has been passed into a record or function
    escapes :: {-# UNPACK #-} !Int
  }
  deriving (Generic)

data SpecificInfo
  = FunctionInfo {-# UNPACK #-} !FunctionInfo'
  | -- | Fields of the record as value x access path pairs
    RecordInfo [(Value, Access)]
  | SelectInfo {-# UNPACK #-} !SelectInfo'
  | -- | Highest field number guaranteed to be selected on all paths
    FormalParamInfo {-# UNPACK #-} !Int
  | ArithPrimInfo {-# UNPACK #-} !ArithPrimInfo'
  | NoSpecificInfo
  deriving (Generic)

data FunctionInfo' = FunctionInfo'
  { formalParams :: [Var],
    body :: Cexp,
    -- | Whether any of the actual parameters are records,
    -- and the size of those records
    arity :: {-# UNPACK #-} !Int,
    calls :: {-# UNPACK #-} !Int
  }
  deriving (Generic)

data SelectInfo' = SelectInfo'
  { record :: {-# UNPACK #-} !Var,
    offset :: {-# UNPACK #-} !Int
  }
  deriving (Generic)

data ArithPrimInfo' = ArithPrimInfo'
  { lowerBound :: {-# UNPACK #-} !Int,
    upperBound :: {-# UNPACK #-} !Int
  }
  deriving (Generic)

-- ~, +, -, *, div, fadd, fsub, fmul, fdiv
arithPrimopInfo :: Primop -> (Bool, Int, Int)
arithPrimopInfo = undefined

use :: Value -> State ContractInfo ()
use (Var v) = at' v % _Just % #used %= (+ 1)
use (Label v) = use (Var v)
use _ = pure ()

escape :: Value -> State ContractInfo ()
escape (Var v) = use (Var v) >> at' v % _Just % #escapes %= (+ 1)
escape (Label v) = escape (Var v)
escape _ = pure ()

call :: Value -> State ContractInfo ()
call (Var v) = do
  use (Var v)
  at' v % _Just % #specific % #_FunctionInfo % #calls %= (+ 1)
call (Label v) = call (Var v)
call _ = pure ()

enter :: Int -> SpecificInfo -> State ContractInfo ()
enter v specific = modify' $ IM.insert v $ Info specific 0 0

enterSimple :: Int -> State ContractInfo ()
enterSimple v = enter v NoSpecificInfo

gatherInfo :: Cexp -> ContractInfo
gatherInfo = flip execState IM.empty . go
  where
    go :: Cexp -> State ContractInfo ()
    go (Fix fns rest) = do
      for_ fns $ \(fn, params, e) -> do
        traverse_ (`enter` FormalParamInfo 0) params
        enter fn $ FunctionInfo $ FunctionInfo' params e 0 0
      go rest
    go (Record fields v rest) = do
      traverse_ (escape . fst) fields
      enter v $ RecordInfo fields
      go rest
    go (Offset _ p v rest) = use p >> enterSimple v >> go rest
    go (App f vs) = call f >> traverse_ escape vs
    go (Select off (Var r) v rest) = do
      use (Var r)
      -- Record highest select offset across all paths
      at' r % _Just % #specific % #_FormalParamInfo %= max off
      enter v $ SelectInfo $ SelectInfo' r off
      go rest
    go Select {} = error "Must select off of record variable"
    go (Switch p branches) = use p >> traverse_ go branches
    go (Primop op ps [v] rest) | (True, lower, upper) <- arithPrimopInfo op = do
      traverse_ use ps
      enter v $ ArithPrimInfo $ ArithPrimInfo' lower upper
      traverse_ go rest
    go (Primop _ ps vs rest) =
      traverse_ use ps >> traverse_ enterSimple vs >> traverse_ go rest

type Env = IM.IntMap Value

-- | A `Var` can refer to another `Var`, and so on and so forth.
-- `rename` follows the chain of `Var` and `Label` links to get
-- the root value of a variable name.
rename :: Value -> State Env Value
rename = gets . go
  where
    go (Var v) env | Just v' <- IM.lookup v env = go v' env
    go (Label v) env | Just v' <- IM.lookup v env = go v' env
    go v _ = v

-- | Adds a binding to the environment.
newname :: Var -> Value -> State Env ()
newname k v = modify' $ IM.insert k v