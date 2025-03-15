{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OrPatterns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NoFieldSelectors #-}

module Miniml.Optimization.Contract (gatherInfo, reduce) where

import Control.Monad (filterM, when)
import Control.Monad.Extra (ifM, notM, unlessM)
import Control.Monad.State.Strict (State, execState, get, gets, modify', runState)
import Data.Foldable (for_, traverse_)
import Data.Functor (($>), (<&>))
import Data.Functor.Foldable (embed, project)
import Data.IntMap qualified as IM
import Data.IntSet qualified as IS
import GHC.Generics (Generic)
import Miniml.Cps (Cexp (..), Value (..), Var)
import Miniml.Shared (Access (Selp), Primop (..))
import Optics (anyOf, at', gplate, traverseOf, (%), (^.), _Just)
import Optics.State.Operators ((%=), (.=), (?=))
import Optics.Zoom (zoom)

type ContractInfo = IM.IntMap Info

data Info = Info
  { specific :: !SpecificInfo,
    -- | The number of times a variable has been referred to.
    used :: {-# UNPACK #-} !Int
  }
  deriving (Show, Generic)

data SpecificInfo
  = FunctionInfo {-# UNPACK #-} !FunctionInfo
  | RecordInfo [(Value, Access)]
  | SelectInfo {-# UNPACK #-} !SelectInfo
  | IfIdiomInfo !Cexp !Cexp
  | NoSpecificInfo
  deriving (Show, Generic)

data FunctionInfo = F
  { formalParams :: [Var],
    body :: !Cexp,
    calls :: {-# UNPACK #-} !Int,
    -- | True if function has not been marked as irreducible by other phases.
    reducible :: !Bool,
    -- | True if function can be reduced with boolean idiom simplification.
    specialUse :: !Bool
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
        enter fn $ FunctionInfo $ F params e 0 (IS.notMember fn irreducible) False
      traverse_ (\(_, _, e) -> go e) fns
      go rest
      for_ fns $ \(fn, _, _) -> do
        -- Handle functions that are if idiom functions.
        info <- get
        case IM.lookup fn info of
          Just (Info (FunctionInfo (F _ (Primop _ [Var c, _] _ [a, b]) 2 True True)) 2)
            | Just (Info _ 1) <- IM.lookup c info -> at' fn ?= Info (IfIdiomInfo a b) 2
          _ -> pure ()
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
    go (Primop _ ps [] rest@[App (Var f) [Int 1], App (Var f') [Int 0]]) | f == f' = do
      gets (IM.lookup f) >>= \case
        Just (Info (FunctionInfo (F [x] (Primop Ieql [Var x', Int 0] _ _) _ _ _)) _)
          | x == x' -> at' f % _Just % #specific % #_FunctionInfo % #specialUse .= True
        _ -> pure ()
      traverse_ use ps >> traverse_ go rest
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
used v = gets $ anyOf (#info % at' v % _Just % #used) (> 0)

lookupInfo :: Value -> State ContractState Info
lookupInfo (Label l) = lookupInfo (Var l)
lookupInfo (Var v) = zoom #info $ gets (IM.! v)
lookupInfo v = error $ "Invalid value: " ++ show v

reduce :: Env -> ContractInfo -> Cexp -> (Int, Cexp)
reduce env0 info0 e0 =
  let (e0', s) =
        flip runState (ContractState env0 info0 0) $
          go e0 >>= traverseOf (gplate @Value) rename
   in (s ^. #clicks, e0')
  where
    go (Select i v w e) =
      ifM
        (notM (used w))
        (click >> go e)
        ( rename v >>= lookupInfo >>= \case
            Info (RecordInfo vl) _ ->
              click >> (newname w =<< rename (fst (vl !! i))) >> go e
            _ -> Select i v w <$> go e
        )
    go e@(Switch v es) =
      rename v >>= \case
        Int i -> click >> go (es !! i)
        _ -> embed <$> traverse go (project e)
    go (Fix [] rest) = click >> go rest
    go (Fix fns rest) = do
      fns' <- filterM (notM . canBeDropped) fns >>= traverse removeUnusedParams
      when (length fns' /= length fns) click
      if null fns'
        then go rest
        else embed <$> traverse go (project (Fix fns' rest))
      where
        removeUnusedParams (f, ps, r) =
          lookupInfo (Var f) >>= \case
            -- Function doesn't escape.
            Info (FunctionInfo (F _ _ calls _ _)) uses | uses == calls -> do
              ps' <- filterM used ps
              when (length ps' /= length ps) click
              pure (f, ps', r)
            _ -> pure (f, ps, r)
        canBeDropped (f, _, _) =
          lookupInfo (Var f) <&> \case
            -- Filter all used == 1 && called == 1 too.
            Info (FunctionInfo (F _ _ 1 True _)) 1 -> True
            Info (IfIdiomInfo {}) _ -> True
            Info _ 0 -> True
            _ -> False
    -- If f's used == 1 && called == 1 then replace with function body.
    go (App f vs) = do
      rename f >>= lookupInfo >>= \case
        Info (FunctionInfo (F ps body 1 True _)) 1 -> do
          vs' <- traverse rename vs
          traverse_ (uncurry newname) =<< filterM (used . fst) (zip ps vs')
          go body
        Info (FunctionInfo (F ps _ calls _ _)) uses
          | uses == calls ->
              fmap (App f . fmap snd) . filterM (used . fst) $ zip ps vs
        _ -> pure $ App f vs
    go (Record paths v a) =
      ifM
        (notM (used v))
        (click >> go a)
        (Record <$> traverse rewritePaths paths <*> pure v <*> go a)
      where
        rewritePaths (Var w, p) =
          rename (Var w) >>= lookupInfo >>= \case
            Info (SelectInfo (S v' i)) _ ->
              click $> (Var v', Selp i p)
            _ -> pure (Var w, p)
        rewritePaths path = pure path
    go e@(Primop op ps [] [App f [Int 1], App f' [Int 0]]) | f == f' = do
      rename f >>= lookupInfo >>= \case
        Info (IfIdiomInfo a b) _ -> go (Primop op ps [] [b, a])
        _ -> embed <$> traverse go (project e)
    go e@(Primop Negate [p] [v] [r]) =
      ifM
        (notM (used v))
        (click >> go r)
        ( rename p >>= \case
            Int i -> click >> newname v (Int (negate i)) >> go r
            _ -> embed <$> traverse go (project e)
        )
    go e@(Primop op [a, b] [v] [r])
      | op `elem` [Plus, Minus, Times, Div, Fadd, Fsub, Fmul, Fdiv] = do
          Info _ n <- lookupInfo (Var v)
          x <- rename a
          y <- rename b
          -- Don't worry about exceptions with arithmetic, rely on overflow
          case (op, x, y) of
            (Times, Int 1, Var _); (Plus, Int 0, Var _) -> const' n y
            (Times, Var _, Int 1); (Div, Var _, Int 1); (Plus, Var _, Int 0); (Minus, Var _, Int 0) -> const' n x
            (Times, Int 0, _); (Times, _, Int 0) -> const' n (Int 0)
            (Times, Int i, Int j) -> const' n (Int (i * j))
            (Div, Int i, Int j) | j /= 0 -> const' n (Int (i `quot` j))
            (Plus, Int i, Int j) -> const' n (Int (i + j))
            (Minus, Int i, Int j) -> const' n (Int (i - j))
            _ -> embed <$> traverse go (project e)
      where
        const' n x = click >> when (n > 0) (newname v x) >> go r
    go e@(Primop op [a, b] [] [t, f]) =
      liftA2 (op,,) (rename a) (rename b) >>= \case
        (Ieql, Int i, Int j) -> click >> if i == j then go t else go f
        (Ineq, Int i, Int j) -> click >> if i /= j then go t else go f
        (Lt, Int i, Int j) -> click >> if i < j then go t else go f
        (Leq, Int i, Int j) -> click >> if i <= j then go t else go f
        (Gt, Int i, Int j) -> click >> if i > j then go t else go f
        (Geq, Int i, Int j) -> click >> if i >= j then go t else go f
        _ -> embed <$> traverse go (project e)
    go e@(Primop _ _ [v] [a]) =
      ifM (notM (used v)) (click >> go a) (embed <$> traverse go (project e))
    go e = embed <$> traverse go (project e)