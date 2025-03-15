{-# LANGUAGE GADTs #-}

module Miniml.Closure.Lift where

import Control.Monad.State.Strict (modify', runState)
import Data.Functor.Foldable (cata, embed)
import Miniml.Cps (Cexp (Fix), CexpF (FixF))

lift :: Cexp -> Cexp
lift = uncurry (flip Fix) . flip runState [] . cata go
  where
    go (FixF fl rest) = do
      fl' <- traverse (\(f, xs, e) -> (f,xs,) <$> e) fl
      modify' (fl' ++)
      rest
    go e = embed <$> sequence e
