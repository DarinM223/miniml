{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NoFieldSelectors #-}

module Miniml.Optimization.Contract (gatherInfo, reduce) where

import Control.Applicative (liftA3)
import Control.Monad (filterM, when)
import Control.Monad.Extra (ifM, notM)
import Control.Monad.State.Strict (State, execState, gets, modify', runState)
import Data.Foldable (for_, traverse_)
import Data.Functor ((<&>))
import Data.Functor.Foldable (cata, embed)
import Data.Generics.Product.Types (types)
import Data.IntMap qualified as IM
import GHC.Generics (Generic)
import Miniml.Cps (Cexp (..), CexpF (..), Value (..), Var)
import Miniml.Shared (Access, Primop (..))
import Optics (at', traverseOf, (%), (^.), _Just)
import Optics.State.Operators ((%=))
import Optics.Zoom (zoom)

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
rename' :: Env -> Value -> Value
rename' env (Var v) | Just v' <- IM.lookup v env = rename' env v'
rename' env (Label v) | Just v' <- IM.lookup v env = rename' env v'
rename' _ v = v

data ContractState = ContractState
  { env :: {-# UNPACK #-} !Env,
    info :: {-# UNPACK #-} !ContractInfo,
    clicks :: {-# UNPACK #-} !Int
  }
  deriving (Generic)

reduce :: Env -> ContractInfo -> Cexp -> (Int, Cexp)
reduce env0 info0 e0 =
  let (e0', s) =
        flip runState (ContractState env0 info0 0) $
          cata go e0 >>= traverseOf (types @Value) rename
   in (s ^. #clicks, e0')
  where
    click :: State ContractState ()
    click = zoom #clicks $ modify' (+ 1)

    rename :: Value -> State ContractState Value
    rename = zoom #env . gets . flip rename'

    newname :: Var -> Value -> State ContractState ()
    newname k v = zoom #env $ modify' $ IM.insert k v

    used :: Var -> State ContractState Bool
    used v = zoom #info $ gets $ \m -> m IM.! v ^. #used > 0

    lookupInfo :: Value -> State ContractState Info
    lookupInfo (Var v) = zoom #info $ gets $ \m -> m IM.! v
    lookupInfo _ = error "Invalid value"

    go :: CexpF (State ContractState Cexp) -> State ContractState Cexp
    go (SelectF i v w e) =
      ifM
        (notM (used w))
        (click >> e)
        ( rename v >>= lookupInfo >>= \case
            Info (RecordInfo vl) _ _ ->
              click >> (newname w =<< rename (fst (vl !! i))) >> e
            _ -> Select i v w <$> e
        )
    go e@(SwitchF v es) =
      rename v >>= \case
        Int i -> click >> es !! i
        _ -> embed <$> sequence e
    go (FixF [] rest) = rest -- TODO(DarinM223): click?
    go (FixF fns rest) = do
      fns' <- filterM (notM . canBeDropped) fns
      if null fns' then rest else embed <$> sequence (FixF fns' rest)
      where
        canBeDropped (f, _, _) =
          lookupInfo (Var f) <&> \case
            -- Filter all used == 1 && called == 1 too.
            Info (FunctionInfo (FunctionInfo' _ _ _ 1)) 1 _ -> True
            Info _ 0 _ -> True
            _ -> False
    -- If f's used == 1 && called == 1 then replace with function body.
    go e@(AppF f vs) =
      rename f >>= lookupInfo >>= \case
        Info (FunctionInfo (FunctionInfo' ps body _ 1)) 1 _ -> do
          click
          vs' <- traverse rename vs
          traverse_ (uncurry newname) $ zip ps vs'
          pure body
        _ -> embed <$> sequence e
    go (RecordF _ v a) =
      ifM
        (notM (used v))
        (click >> a)
        undefined
    go e@(PrimopF Negate [p] [v] [r]) =
      ifM
        (notM (used v))
        (click >> r)
        ( rename p >>= \case
            Int i -> click >> newname v (Int (negate i)) >> r
            _ -> embed <$> sequence e
        )
    go e@(PrimopF op [a, b] [v] [r])
      | op `elem` [Plus, Minus, Times, Div, Fadd, Fsub, Fmul, Fdiv] =
          liftA3 (op,,,) (rename a) (rename b) (lookupInfo (Var v)) >>= \case
            (Times, Int 1, x@(Var _), Info _ n _) -> const' n x
            (Times, x@(Var _), Int 1, Info _ n _) -> const' n x
            (Times, Int 0, _, Info _ n _) -> const' n (Int 0)
            (Times, _, Int 0, Info _ n _) -> const' n (Int 0)
            (Times, Int i, Int j, Info (ArithPrimInfo (ArithPrimInfo' l u)) n _) ->
              constFold (i * j) l u n
            (Div, x@(Var _), Int 1, Info _ n _) -> const' n x
            (Div, Int i, Int j, Info (ArithPrimInfo (ArithPrimInfo' l u)) n _)
              | j /= 0 -> constFold (i `quot` j) l u n
            (Plus, x@(Var _), Int 0, Info _ n _) -> const' n x
            (Plus, Int 0, x@(Var _), Info _ n _) -> const' n x
            (Plus, Int i, Int j, Info (ArithPrimInfo (ArithPrimInfo' l u)) n _) ->
              constFold (i + j) l u n
            (Minus, x@(Var _), Int 0, Info _ n _) -> const' n x
            (Minus, Int i, Int j, Info (ArithPrimInfo (ArithPrimInfo' l u)) n _) ->
              constFold (i - j) l u n
            _ -> embed <$> sequence e
      where
        const' n x = click >> when (n > 0) (newname v x) >> r
        constFold result lower upper numUsed
          | result <= upper && result >= lower = const' numUsed (Int result)
          | otherwise = embed <$> sequence e
    go e@(PrimopF _ _ [v] [a]) =
      ifM (notM (used v)) (click >> a) (embed <$> sequence e)
    go e = embed <$> sequence e