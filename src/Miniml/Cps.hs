{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Miniml.Cps where

import Control.Applicative (liftA2, liftA3)
import Control.Monad (replicateM)
import Control.Monad.Cont (ContT (ContT, runContT))
import Control.Monad.State.Strict (MonadState, State)
import Data.Char (ord)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Text (Text)
import Data.Text qualified as T
import Miniml.Lambda qualified as L
import Miniml.Shared (Access (..), Primop (Callcc, Gethdlr, Sethdlr, Throw), fresh)

type Var = Int

data Value
  = Var Var
  | Label Var
  | Int Int
  | Real Text
  | String Text
  deriving (Show, Eq)

data Cexp
  = Record [(Value, Access)] Var Cexp
  | Select Int Value Var Cexp
  | Offset Int Value Var Cexp
  | App Value [Value]
  | Fix [(Var, [Var], Cexp)] Cexp
  | Switch Value [Cexp]
  | Primop Primop [Value] [Var] [Cexp]
  deriving (Show)

makeBaseFunctor ''Cexp

data PrimopResult = OneResult | NoResult | Branching

primopResultType :: Primop -> PrimopResult
primopResultType = undefined

primopNumArgs :: Primop -> Int
primopNumArgs = undefined

primopArgs :: Applicative f => Primop -> L.Lexp -> (L.Lexp -> f a) -> f [a]
primopArgs i e go = case primopNumArgs i of
  1 -> (: []) <$> go e
  _ -> case e of
    L.Record a -> traverse go a
    _ -> error $ "Multi argument primop " ++ show i ++ " expecting record argument"

newtype ConvertM a = ConvertM {unConvertM :: State Int a}
  deriving (Functor, Applicative, Monad, MonadState Int)

instance MonadFail ConvertM where
  fail :: String -> ConvertM a
  fail = error

convert :: L.Lexp -> State Int (Var, Cexp)
convert lexp = unConvertM $ do
  k <- fresh
  fmap (k,) $ runContT (go lexp) $ \v -> pure $ App (Var k) [v]
  where
    go :: L.Lexp -> ContT Cexp ConvertM Value
    go (L.Var v) = pure $ Var v
    go (L.Real r) = pure $ Real r
    go (L.Int i) = pure $ Int i
    go (L.String s)
      | T.length s == 1 = pure $ Int $ ord $ T.head s
      | otherwise = pure $ String s
    go (L.Record []) = pure $ Int 0
    go (L.Record a) = do
      a' <- traverse go a
      x <- fresh
      ContT $ \c -> Record (fmap (,Offp 0) a') x <$> c (Var x)
    go (L.Select i e) = do
      v <- go e
      w <- fresh
      ContT $ \c -> Select i v w <$> c (Var w)
    go (L.Prim i) = do
      x <- fresh
      go $ L.Fn x $ L.App (L.Prim i) (L.Var x)
    go (L.App (L.Prim Callcc) f) = ContT $ \c -> do
      [k, k', x, x', h] <- replicateM 5 fresh
      kf <- (k,[x],) <$> c (Var x)
      let kf' = (k', [x'], Primop Sethdlr [Var h] [] [App (Var k) [Var x']])
      Primop Gethdlr [] [h] . (: []) . Fix [kf, kf']
        <$> runContT (go f) (\v -> pure $ App v [Var k', Var k])
    go (L.App (L.Prim Throw) e) = do
      k <- go e
      (f, x, j) <- liftA3 (,,) fresh fresh fresh
      ContT $ \c -> Fix [(f, [x, j], App k [Var x])] <$> c (Var f)
    go (L.App (L.Prim i) e) = case primopResultType i of
      OneResult -> do
        a <- primopArgs i e go
        w <- fresh
        ContT $ \c -> Primop i a [w] . (: []) <$> c (Var w)
      NoResult -> do
        a <- primopArgs i e go
        ContT $ \c -> Primop i a [] . (: []) <$> c (Int 0)
      Branching -> do
        a <- primopArgs i e go
        (w, k, x) <- liftA3 (,,) fresh fresh fresh
        ContT $ \c -> do
          kf <- (k,[x],) <$> c (Var x)
          pure $ Fix [kf] (Primop i a [w] [App (Var k) [Int 0], App (Var k) [Int 1]])
    go (L.Fn v e) = do
      z <- go e
      (f, k) <- liftA2 (,) fresh fresh
      ContT $ \c -> Fix [(f, [v, k], App (Var k) [z])] <$> c (Var f)
    go (L.App f e) = do
      f' <- go f
      e' <- go e
      (r, x) <- liftA2 (,) fresh fresh
      ContT $ \c -> do
        rf <- (r,[x],) <$> c (Var x)
        pure $ Fix [rf] (App f' [e', Var r])
    go (L.Fix h b e) = do
      let g (h', L.Fn v b') = do
            w <- fresh
            (h',[v, w],) <$> runContT (go b') (\z -> pure (App (Var w) [z]))
          g _ = error "Fix must have expressions of type Fn"
      ContT $ \c -> Fix <$> traverse g (zip h b) <*> runContT (go e) c
    go (L.Con (L.Constant i) _) = go $ L.Int i
    go (L.Con (L.Tagged i) e) = go $ L.Record [e, L.Int i]
    go (L.Con L.Transparent e) = go e
    go (L.Con L.TransB e) = go e
    go (L.Con L.TransU e) = go e
    go (L.Con (L.Variable v p) e) = do
      w <- go e
      x <- fresh
      ContT $ \c -> Record [(w, Offp 0), (Var v, p)] x <$> c (Var x)
    go (L.Con (L.VariableC v p) _) = do
      (w, x) <- liftA2 (,) fresh fresh
      ContT $ \c -> Record [(Var v, p)] w . Select 0 (Var w) x <$> c (Var x)
    go (L.Con L.Undecided _) = error "Con cannot be applied to Undecided"
    go (L.Con L.Ref _) = error "Con cannot be applied to Ref"
    go (L.Decon (L.Tagged _) e) = go $ L.Select 0 e
    go (L.Decon L.Transparent e) = go e
    go (L.Decon L.TransB e) = go e
    go (L.Decon L.TransU e) = go e
    go (L.Decon (L.Variable _ _) e) = go $ L.Select 0 e
    go (L.Decon L.Undecided _) = error "Decon cannot be applied to Undecided"
    go (L.Decon (L.VariableC v p) _) =
      error $ "Decon cannot be applied to VariableC: " ++ show v ++ ", " ++ show p
    go (L.Decon L.Ref _) = error "Decon cannot be applied to Ref"
    go (L.Decon (L.Constant i) _) =
      error $ "Decon cannot be applied to Constant " ++ show i
    go (L.Switch e l [a@(L.DataCon (L.Constant 0), _), b@(L.DataCon (L.Constant 1), _)] Nothing) =
      go (L.Switch e l [b, a] Nothing)
    go (L.Switch u _ l _) = do
      u' <- go u
      (k, x) <- liftA2 (,) fresh fresh
      let g (_, le) = runContT (go le) (\z -> pure (App (Var k) [z]))
      ContT $ \c -> do
        kf <- (k,[x],) <$> c (Var x)
        Fix [kf] . Switch u' <$> traverse g l
    go (L.Handle a b) = ContT $ \c -> do
      [h, k, n, x, e] <- replicateM 5 fresh
      kf <- (k,[x],) <$> c (Var x)
      nf <-
        (n,[e],) . Primop Sethdlr [Var h] [] . (: [])
          <$> runContT (go b) (pure . flip App [Var e, Var k])
      (Primop Gethdlr [] [h] . (: []))
        . Fix [kf, nf]
        . (Primop Sethdlr [Var n] [] . (: []))
        <$> runContT
          (go a)
          (\v -> pure $ Primop Sethdlr [Var h] [] [App (Var k) [v]])
    go (L.Raise e) = do
      e' <- go e
      h <- fresh
      ContT $ \_ -> pure $ Primop Gethdlr [] [h] [App (Var h) [e']]