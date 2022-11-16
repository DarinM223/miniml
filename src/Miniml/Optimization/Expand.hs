{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NoFieldSelectors #-}

module Miniml.Optimization.Expand (gatherInfo, expand) where

import Control.Applicative (liftA2)
import Control.Monad.State.Strict (State, execState, modify', runState, state)
import Data.Bifunctor (first)
import Data.Foldable (foldl', for_, traverse_)
import Data.Functor.Foldable (embed, project)
import Data.IntMap.Strict qualified as IM
import GHC.Generics (Generic)
import Miniml.Cps (Cexp (..), Value (Int, Label, Var), Var)
import Miniml.Shared (fresh)
import Optics (at', zoom, (%), (%~), (&), _Just)
import Optics.State.Operators ((%=))

data Info
  = FunctionInfo {-# UNPACK #-} !FunctionInfo
  | ArgumentInfo {-# UNPACK #-} !ArgumentInfo
  | RecordInfo {-# UNPACK #-} !RecordInfo
  deriving (Show, Generic)

data FunctionInfo = F
  { args :: ![Var],
    body :: !Cexp,
    escapes :: {-# UNPACK #-} !Int,
    calls :: {-# UNPACK #-} !Int,
    size :: {-# UNPACK #-} !Int
  }
  deriving (Show, Generic)

data ArgumentInfo = A
  { escapes :: {-# UNPACK #-} !Int,
    numSelects :: {-# UNPACK #-} !Int,
    potentialIntConstSavings :: {-# UNPACK #-} !Int
  }
  deriving (Show, Generic)

data RecordInfo = R
  { escapes :: {-# UNPACK #-} !Int,
    numFields :: {-# UNPACK #-} !Int
  }
  deriving (Show, Generic)

type ExpandInfo = IM.IntMap Info

constC :: Int
constC = 50

constD :: Int
constD = 20

constE :: Int
constE = 50

call :: Value -> State ExpandInfo ()
call (Label v) = call (Var v)
call (Var v) = at' v % _Just % #_FunctionInfo % #calls %= (+ 1)
call _ = pure ()

escape :: Value -> State ExpandInfo ()
escape (Label v) = escape (Var v)
escape (Var v) = at' v % _Just %= incEscapes
  where
    incEscapes = \case
      FunctionInfo i -> FunctionInfo $ i & #escapes %~ (+ 1)
      ArgumentInfo i -> ArgumentInfo $ i & #escapes %~ (+ 1)
      RecordInfo i -> RecordInfo $ i & #escapes %~ (+ 1)
escape _ = pure ()

select :: Value -> State ExpandInfo ()
select (Label v) = escape (Var v)
select (Var v) = at' v % _Just % #_ArgumentInfo % #numSelects %= (+ 1)
select _ = pure ()

incSave :: Value -> Int -> State ExpandInfo ()
incSave (Label v) i = incSave (Var v) i
incSave (Var v) i =
  at' v % _Just % #_ArgumentInfo % #potentialIntConstSavings %= (+ i)
incSave _ _ = pure ()

enter :: Var -> Info -> State ExpandInfo ()
enter v = modify' . IM.insert v

gatherInfo :: Cexp -> ExpandInfo
gatherInfo = flip execState IM.empty . go
  where
    go (Record vl w e) = do
      traverse_ (escape . fst) vl
      enter w $ RecordInfo (R 0 (length vl))
      go e
    go (Select _ v _ e) = select v >> go e
    go (Offset _ _ _ e) = go e
    go (App f vl) = call f >> traverse_ escape vl
    go (Fix fl e) = do
      for_ fl $ \(f, xs, body) -> do
        traverse_ (\x -> enter x (ArgumentInfo (A 0 0 0))) xs
        enter f $ FunctionInfo $ F xs body 0 0 (bodySize body)
      traverse_ (\(_, _, body) -> go body) fl
      go e
    go (Switch v el) =
      incSave v (jumpSavings 4 el) >> traverse_ go el
    go (Primop op [Int i, v] [] el@[_, _]) =
      go (Primop op [v, Int i] [] el)
    go (Primop _ [v, Int _] [] el@[_, _]) =
      incSave v (jumpSavings 2 el) >> traverse_ go el
    go (Primop _ vl wl el) = do
      let isConst (Int _) = True
          isConst _ = False
          potential = length vl + length wl
          savings = case length (filter (not . isConst) vl) of
            1 -> potential
            2 -> potential `quot` 4
            _ -> 0
      traverse_ (`incSave` savings) vl
      traverse_ go el

jumpSavings :: Int -> [Cexp] -> Int
jumpSavings k el = k + ((n - 1) `quot` n) * sum (fmap bodySize el)
  where
    n = length el

bodySize :: Cexp -> Int
bodySize (Record v _ b) = length v + 2 + bodySize b
bodySize (Select _ _ _ b) = 1 + bodySize b
bodySize (Offset _ _ _ b) = 1 + bodySize b
bodySize (App _ a) = 1 + length a
bodySize (Fix fl e) = sum (fmap (\(_, _, b) -> bodySize b + 1) fl) + bodySize e
bodySize (Switch _ bs) = 4 + sum (fmap (\b -> bodySize b + 1) bs)
bodySize (Primop _ v w cs) = length v + length w + sum (fmap bodySize cs)

argumentSavings :: ExpandInfo -> Value -> Var -> Int
argumentSavings info (Int _) p =
  case IM.lookup p info of
    Just (ArgumentInfo (A {potentialIntConstSavings = savings})) -> savings
    _ -> 0
argumentSavings info (Var a) p =
  case liftA2 (,) (IM.lookup a info) (IM.lookup p info) of
    Just (FunctionInfo (F {escapes = 1}), ArgumentInfo (A {escapes = 0})) -> 6
    Just
      ( RecordInfo (R {escapes = 1, numFields}),
        ArgumentInfo (A {escapes = 0, numSelects = n})
        ) -> numFields + 2 + n
    Just (RecordInfo _, ArgumentInfo (A {numSelects = n})) -> n
    _ -> 0
argumentSavings _ _ _ = 0

-- | Alpha renaming expression.
rename :: IM.IntMap Value -> Cexp -> State Int Cexp
rename = go
  where
    look env (Label l) | Just l' <- IM.lookup l env = l'
    look env (Var v) | Just v' <- IM.lookup v env = v'
    look _ v = v

    go env (Record vl w e) = do
      w' <- fresh
      Record (first (look env) <$> vl) w' <$> go (IM.insert w (Var w') env) e
    go env (Select i v w e) = do
      w' <- fresh
      Select i (look env v) w' <$> go (IM.insert w (Var w') env) e
    go env (Offset i v w e) = do
      w' <- fresh
      Offset i (look env v) w' <$> go (IM.insert w (Var w') env) e
    go env (App f xs) = pure $ App (look env f) (look env <$> xs)
    go env (Fix fl rest) = do
      fl' <- traverse fn fl
      let env' = foldl' (uncurry . updateEnv) env $ zip fl fl'
      Fix
        <$> traverse (\(f, xs, e) -> (f,xs,) <$> go env' e) fl'
        <*> go env' rest
      where
        fn (_, xs, e) = (,,e) <$> fresh <*> traverse (const fresh) xs
        updateEnv env' (f, xs, _) (f', xs', _) =
          let env'' = IM.insert f (Var f') env'
           in foldl' (\e (x, x') -> IM.insert x (Var x') e) env'' $ zip xs xs'
    go env (Switch v es) = Switch (look env v) <$> traverse (go env) es
    go env (Primop op vs ws es) = do
      ws' <- traverse (const fresh) ws
      let env' = foldl' (\e (w, w') -> IM.insert w (Var w') e) env $ zip ws ws'
      Primop op (fmap (look env) vs) ws' <$> traverse (go env') es

data ExpandState = ExpandState
  { env :: !(IM.IntMap Value),
    counter :: {-# UNPACK #-} !Int,
    clicks :: {-# UNPACK #-} !Int
  }
  deriving (Generic)

expand :: Int -> ExpandInfo -> Cexp -> State Int (Int, Cexp)
expand r info e0 = state $ \(!tmp) ->
  let (e0', ExpandState _ !tmp' !clicks) =
        runState (go 0 e0) (ExpandState IM.empty tmp 0)
   in ((clicks, e0'), tmp')
  where
    get (Label v) = get (Var v)
    get (Var v) = IM.lookup v info
    get _ = Nothing

    go :: Int -> Cexp -> State ExpandState Cexp
    go level (App f xs) = do
      case get f of
        Just (FunctionInfo (F args body _ calls size))
          | calls > 0 && heuristic < constC - level * constD - r * constE -> do
              #clicks %= (+ 1)
              go (level + 1) =<< zoom #counter (rename env body)
          where
            heuristic = size - savings - (1 + length args) - size `quot` calls
            savings = sum $ uncurry (argumentSavings info) <$> zip xs args
            env = IM.fromList $ zip args xs
        _ -> pure (App f xs)
    go level e = embed <$> traverse (go level) (project e)