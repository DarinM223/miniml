{-# LANGUAGE OverloadedLabels #-}

module Miniml.Optimization.Contract where

import Control.Monad.State.Strict (State, execState, modify')
import Data.Foldable (for_, traverse_)
import Data.IntMap qualified as IM
import GHC.Generics (Generic)
import Miniml.Cps (Cexp (..), Value (Var), Var)
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
    arity :: {-# UNPACK #-} !Int
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

arithPrimopInfo :: Primop -> (Bool, Int, Int)
arithPrimopInfo = undefined

gatherInfo :: Cexp -> ContractInfo
gatherInfo = flip execState IM.empty . go
  where
    recordUse :: Value -> State ContractInfo ()
    recordUse (Var v) = at' v % _Just % #used %= (+ 1)
    recordUse _ = pure ()

    recordEscape :: Value -> State ContractInfo ()
    recordEscape (Var v) = do
      recordUse (Var v)
      at' v % _Just % #escapes %= (+ 1)
    recordEscape _ = pure ()

    addGenericVar :: Var -> State ContractInfo ()
    addGenericVar v = modify' $ IM.insert v $ Info NoSpecificInfo 0 0

    go :: Cexp -> State ContractInfo ()
    go (Fix fns rest) = do
      for_ fns $ \(fn, params, e) -> do
        for_ params $ \param ->
          modify' $ IM.insert param $ Info (FormalParamInfo 0) 0 0
        modify' $
          IM.insert fn $
            Info (FunctionInfo (FunctionInfo' params e 0)) 0 0
      go rest
    go (Record fields v rest) = do
      traverse_ (recordEscape . fst) fields
      modify' $ IM.insert v $ Info (RecordInfo fields) 0 0
      go rest
    go (Offset _ p v rest) = recordUse p >> addGenericVar v >> go rest
    go (App f vs) = do
      recordUse f
      traverse_ recordEscape vs
    go (Select off (Var r) v rest) = do
      recordUse (Var r)
      -- Record highest select offset across all paths
      at' r % _Just % #specific % #_FormalParamInfo %= max off
      modify' $ IM.insert v $ Info (SelectInfo (SelectInfo' r off)) 0 0
      go rest
    go Select {} = error "Must select off of record variable"
    go (Switch p branches) = recordUse p >> traverse_ go branches
    go (Primop op ps [v] rest) | (True, lower, upper) <- arithPrimopInfo op = do
      traverse_ recordUse ps
      modify' $
        IM.insert v $
          Info (ArithPrimInfo (ArithPrimInfo' lower upper)) 0 0
      traverse_ go rest
    go (Primop _ ps vs rest) =
      traverse_ recordUse ps >> traverse_ addGenericVar vs >> traverse_ go rest