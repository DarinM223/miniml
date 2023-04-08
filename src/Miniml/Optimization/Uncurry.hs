{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ViewPatterns #-}

module Miniml.Optimization.Uncurry (reduce) where

import Control.Monad.State.Strict (State, runState, state)
import Data.Foldable (foldrM)
import Data.Functor ((<&>))
import Data.Functor.Foldable (embed, project)
import Data.List.Extra (unsnoc)
import GHC.Generics (Generic)
import Miniml.Cps (Cexp (App, Fix), Value (Var), Var, var)
import Miniml.Shared (fresh)
import Optics (elemOf, gplate, zoom, (%))
import Optics.State.Operators ((%=))

freeIn :: Var -> Cexp -> Bool
freeIn = elemOf (gplate @Value % var)

data UncurryState = UncurryState
  { counter :: {-# UNPACK #-} !Int,
    clicks :: {-# UNPACK #-} !Int
  }
  deriving (Generic)

reduce :: Int -> Cexp -> State Int (Int, Cexp)
reduce maxRegisters e0 = state $ \(!tmp) ->
  let (e0', UncurryState !tmp' !clicks) = runState (go e0) (UncurryState tmp 0)
   in ((clicks, e0'), tmp')
  where
    go (Fix fl e) =
      Fix <$> foldrM (\fd acc -> uncurryFn fd <&> (++ acc)) [] fl <*> go e
    go e = embed <$> traverse go (project e)

    uncurryFn :: (Var, [Var], Cexp) -> State UncurryState [(Var, [Var], Cexp)]
    uncurryFn (f, unsnoc -> Just (vs, c), Fix [(g, us, e)] (App (Var c') [Var g']))
      | g == g' && c == c' && not (freeIn c e || freeIn g e) && check (vs ++ us) = do
          us' <- traverse (const fresh') us
          vs' <- traverse (const fresh') vs
          c'' <- fresh'
          g'' <- fresh'
          f' <- fresh'
          #clicks %= (+ 1)
          let fn =
                ( f,
                  vs' ++ [c''],
                  Fix
                    [(g'', us', App (Var f') (Var <$> (vs' ++ us')))]
                    (App (Var c'') [Var g''])
                )
          (fn :) <$> uncurryFn (f', vs ++ us, e)
    uncurryFn (f, vs, e) = (: []) . (f,vs,) <$> go e

    fresh' = zoom #counter fresh
    check l = length l <= maxRegisters