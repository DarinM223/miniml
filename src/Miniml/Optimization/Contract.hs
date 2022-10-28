{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NoFieldSelectors #-}

module Miniml.Optimization.Contract (gatherInfo, reduce) where

import Control.Applicative (liftA2)
import Control.Monad (filterM, when)
import Control.Monad.Extra (ifM, notM, unlessM)
import Control.Monad.State.Strict (State, execState, gets, modify', runState)
import Data.Foldable (for_, traverse_)
import Data.Functor (($>), (<&>))
import Data.Functor.Foldable (cata, embed)
import Data.Generics.Product.Types (types)
import Data.IntMap qualified as IM
import Data.IntSet qualified as IS
import GHC.Generics (Generic)
import Miniml.Cps (Cexp (..), CexpF (..), Value (..), Var)
import Miniml.Shared (Access (Selp), Primop (..))
import Optics (at', traverseOf, (%), (^.), _Just)
import Optics.State.Operators ((%=))
import Optics.Zoom (zoom)

type ContractInfo = IM.IntMap Info

data Info = Info
  { specific :: !SpecificInfo,
    -- | number of times a variable has been referred to
    used :: {-# UNPACK #-} !Int
  }
  deriving (Show, Generic)

data SpecificInfo
  = FunctionInfo {-# UNPACK #-} !FunctionInfo
  | RecordInfo [(Value, Access)]
  | SelectInfo {-# UNPACK #-} !SelectInfo
  | NoSpecificInfo
  deriving (Show, Generic)

data FunctionInfo = F
  { formalParams :: [Var],
    body :: !Cexp,
    calls :: {-# UNPACK #-} !Int,
    -- | True if function has not been marked as irreducible by other phases
    reducible :: !Bool
  }
  deriving (Show, Generic)

data SelectInfo = S
  { record :: {-# UNPACK #-} !Var,
    offset :: {-# UNPACK #-} !Int
  }
  deriving (Show, Generic)

use :: Value -> State ContractInfo ()
use (Var v) = do
  unlessM (gets (IM.member v)) $ enterSimple v
  at' v % _Just % #used %= (+ 1)
use (Label v) = use (Var v)
use _ = pure ()

call :: Value -> State ContractInfo ()
call (Var v) = do
  use (Var v)
  at' v % _Just % #specific % #_FunctionInfo % #calls %= (+ 1)
call (Label v) = call (Var v)
call _ = pure ()

enter :: Int -> SpecificInfo -> State ContractInfo ()
enter v specific = modify' $ IM.insert v $ Info specific 0

enterSimple :: Int -> State ContractInfo ()
enterSimple v = enter v NoSpecificInfo

gatherInfo :: IS.IntSet -> Cexp -> ContractInfo
gatherInfo irreducible = flip execState IM.empty . go
  where
    go (Fix fns rest) = do
      for_ fns $ \(fn, params, e) -> do
        traverse_ enterSimple params
        enter fn $ FunctionInfo $ F params e 0 (not (IS.member fn irreducible))
        go e
      go rest
    go (Record fields v rest) = do
      traverse_ (use . fst) fields
      enter v $ RecordInfo fields
      go rest
    go (Offset _ p v rest) = use p >> enterSimple v >> go rest
    go (App f vs) = call f >> traverse_ use vs
    go (Select off (Var r) v rest) = do
      use (Var r)
      enter v $ SelectInfo $ S r off
      go rest
    go Select {} = error "Must select off of record variable"
    go (Switch p branches) = use p >> traverse_ go branches
    go (Primop _ ps vs rest) =
      traverse_ use ps >> traverse_ enterSimple vs >> traverse_ go rest

type Env = IM.IntMap Value

data ContractState = ContractState
  { env :: !Env,
    info :: !ContractInfo,
    clicks :: {-# UNPACK #-} !Int
  }
  deriving (Generic)

click :: State ContractState ()
click = #clicks %= (+ 1)

-- | A `Var` can refer to another `Var`, and so on and so forth.
-- `rename` follows the chain of `Var` and `Label` links to get
-- the root value of a variable name.
rename :: Value -> State ContractState Value
rename = zoom #env . gets . flip go
  where
    go env (Var v) | Just v' <- IM.lookup v env = go env v'
    go env (Label v) | Just v' <- IM.lookup v env = go env v'
    go _ v = v

newname :: Var -> Value -> State ContractState ()
newname k v = #env %= IM.insert k v

used :: Var -> State ContractState Bool
used v = zoom #info $ gets $ \m -> m IM.! v ^. #used > 0

lookupInfo :: Value -> State ContractState Info
lookupInfo (Var v) = zoom #info $ gets $ \m -> m IM.! v
lookupInfo v = error $ "Invalid value: " ++ show v

reduce :: Env -> ContractInfo -> Cexp -> (Int, Cexp)
reduce env0 info0 e0 =
  let (e0', s) =
        flip runState (ContractState env0 info0 0) $
          cata go e0 >>= traverseOf (types @Value) rename
   in (s ^. #clicks, e0')
  where
    go :: CexpF (State ContractState Cexp) -> State ContractState Cexp
    go (SelectF i v w e) =
      ifM
        (notM (used w))
        (click >> e)
        ( rename v >>= lookupInfo >>= \case
            Info (RecordInfo vl) _ ->
              click >> (newname w =<< rename (fst (vl !! i))) >> e
            _ -> Select i v w <$> e
        )
    go e@(SwitchF v es) =
      rename v >>= \case
        Int i -> click >> es !! i
        _ -> embed <$> sequence e
    go (FixF [] rest) = click >> rest
    go (FixF fns rest) = do
      fns' <- filterM (notM . canBeDropped) fns >>= traverse removeUnusedParams
      when (length fns' /= length fns) click
      if null fns' then rest else embed <$> sequence (FixF fns' rest)
      where
        removeUnusedParams (f, ps, r) =
          lookupInfo (Var f) >>= \case
            -- Function doesn't escape.
            Info (FunctionInfo (F _ _ calls _)) uses | uses == calls -> do
              ps' <- filterM used ps
              when (length ps' /= length ps) click
              pure (f, ps', r)
            _ -> pure (f, ps, r)
        canBeDropped (f, _, _) =
          lookupInfo (Var f) <&> \case
            -- Filter all used == 1 && called == 1 too.
            Info (FunctionInfo (F _ _ 1 True)) 1 -> True
            Info _ 0 -> True
            _ -> False
    -- If f's used == 1 && called == 1 then replace with function body.
    go (AppF f vs) = do
      rename f >>= lookupInfo >>= \case
        Info (FunctionInfo (F ps body 1 True)) 1 -> do
          vs' <- traverse rename vs
          traverse_ (uncurry newname) =<< filterM (used . fst) (zip ps vs')
          pure body
        Info (FunctionInfo (F ps _ calls _)) uses
          | uses == calls ->
              fmap (App f . fmap snd) . filterM (used . fst) $ zip ps vs
        _ -> pure $ App f vs
    go (RecordF paths v a) =
      ifM
        (notM (used v))
        (click >> a)
        (Record <$> traverse rewritePaths paths <*> pure v <*> a)
      where
        rewritePaths (Var w, p) =
          rename (Var w) >>= lookupInfo >>= \case
            Info (SelectInfo (S v' i)) _ ->
              click $> (Var v', Selp i p)
            _ -> pure (Var w, p)
        rewritePaths path = pure path
    go e@(PrimopF Negate [p] [v] [r]) =
      ifM
        (notM (used v))
        (click >> r)
        ( rename p >>= \case
            Int i -> click >> newname v (Int (negate i)) >> r
            _ -> embed <$> sequence e
        )
    go e@(PrimopF op [a, b] [v] [r])
      | op `elem` [Plus, Minus, Times, Div, Fadd, Fsub, Fmul, Fdiv] = do
          Info _ n <- lookupInfo (Var v)
          -- Don't worry about exceptions with arithmetic, rely on overflow
          liftA2 (op,,) (rename a) (rename b) >>= \case
            (Times, Int 1, x@(Var _)) -> const' n x
            (Times, x@(Var _), Int 1) -> const' n x
            (Times, Int 0, _) -> const' n (Int 0)
            (Times, _, Int 0) -> const' n (Int 0)
            (Times, Int i, Int j) -> const' n (Int (i * j))
            (Div, x@(Var _), Int 1) -> const' n x
            (Div, Int i, Int j) | j /= 0 -> const' n (Int (i `quot` j))
            (Plus, x@(Var _), Int 0) -> const' n x
            (Plus, Int 0, x@(Var _)) -> const' n x
            (Plus, Int i, Int j) -> const' n (Int (i + j))
            (Minus, x@(Var _), Int 0) -> const' n x
            (Minus, Int i, Int j) -> const' n (Int (i - j))
            _ -> embed <$> sequence e
      where
        const' n x = click >> when (n > 0) (newname v x) >> r
    go e@(PrimopF op [a, b] [] [t, f]) =
      liftA2 (op,,) (rename a) (rename b) >>= \case
        (Ieql, Int i, Int j) -> click >> if i == j then t else f
        (Ineq, Int i, Int j) -> click >> if i /= j then t else f
        (Lt, Int i, Int j) -> click >> if i < j then t else f
        (Leq, Int i, Int j) -> click >> if i <= j then t else f
        (Gt, Int i, Int j) -> click >> if i > j then t else f
        (Geq, Int i, Int j) -> click >> if i >= j then t else f
        _ -> embed <$> sequence e
    go e@(PrimopF _ _ [v] [a]) =
      ifM (notM (used v)) (click >> a) (embed <$> sequence e)
    go e = embed <$> sequence e