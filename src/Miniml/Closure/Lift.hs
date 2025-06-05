{-# LANGUAGE GADTs #-}

module Miniml.Closure.Lift where

import Control.Monad.State.Strict (State, modify', runState)
import Data.Functor.Foldable (cata, embed)
import Miniml.Cps (Cexp (Fix), CexpF (FixF), Var)

lift :: Cexp -> Cexp
lift = uncurry (flip Fix) . flip runState [] . cata go
  where
    go :: CexpF (State [(Var, [Var], Cexp)] Cexp) -> State [(Var, [Var], Cexp)] Cexp
    go (FixF fl rest) = do
      fl' <- traverse (\(f, xs, e) -> (f,xs,) <$> e) fl
      modify' (fl' ++)
      rest
    go e = embed <$> sequence e
