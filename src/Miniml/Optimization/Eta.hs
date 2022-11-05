{-# LANGUAGE OverloadedLabels #-}

module Miniml.Optimization.Eta (reduce) where

import Control.Monad.State.Strict (State, gets, runState)
import Data.Functor.Foldable (embed, project)
import Data.IntMap qualified as IM
import GHC.Generics (Generic)
import Miniml.Cps (Cexp (App, Fix), Value (Label, Var), Var)
import Optics (gplate, zoom)
import Optics.State.Operators ((%=))
import Optics.Traversal (traverseOf)

data EtaState = EtaState
  { env :: !(IM.IntMap Value),
    clicks :: {-# UNPACK #-} !Int
  }
  deriving (Show, Generic)

rename :: Value -> State EtaState Value
rename = zoom #env . gets . flip go
  where
    go env (Var v) | Just v' <- IM.lookup v env = go env v'
    go env (Label v) | Just v' <- IM.lookup v env = go env v'
    go _ v = v

newname :: Var -> Value -> State EtaState ()
newname k v = #env %= IM.insert k v

reduce :: Cexp -> (Int, Cexp)
reduce e0 = (clicks, e0')
  where
    (e0', EtaState _ clicks) =
      runState
        (go e0 >>= traverseOf (gplate @Value) rename)
        (EtaState IM.empty 0)

    go (Fix fns body) = Fix <$> etaElim fns <*> go body
    go e = embed <$> traverse go (project e)

    etaElim :: [(Var, [Var], Cexp)] -> State EtaState [(Var, [Var], Cexp)]
    etaElim ((f, ps, body) : rest) = do
      body' <- go body
      case body' of
        App g ps' | (Var <$> ps) == ps' -> do
          g' <- rename g
          if g' /= Var f
            then #clicks %= (+ 1) >> newname f g' >> etaElim rest
            else ((f, ps, body') :) <$> etaElim rest
        _ -> ((f, ps, body') :) <$> etaElim rest
    etaElim [] = pure []