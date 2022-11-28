{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}

module Miniml.Optimization.Cse where

import Control.Monad.State.Strict (State, gets, runState)
import Data.Foldable (traverse_)
import Data.Functor.Foldable (embed, project)
import Data.Generics.Product.Types (typesUsing)
import Data.HashMap.Strict qualified as HM
import Data.IntMap.Strict qualified as IM
import Data.Maybe (fromJust)
import GHC.Generics (Generic)
import Miniml.Cps (Cexp (..), Value (Label, Var), Var)
import Miniml.Optimization.CataExample (Shallow)
import Miniml.Optimization.Hoist (bindings, constr)
import Optics (at', traverseOf, use, zoom, (%), (^.))
import Optics.State.Operators ((%=))

common :: Cexp -> Cexp
common (Record fl _ _) = Record fl 0 (App (Var 0) [])
common (Select i v _ _) = Select i v 0 (App (Var 0) [])
common (Offset i v _ _) = Offset i v 0 (App (Var 0) [])
common (App f vl) = App f vl
common (Fix _ _) = Fix [] (App (Var 0) [])
common (Switch v _) = Switch v []
common (Primop op vl wl _) = Primop op vl (0 <$ wl) []

data CseState = CseState
  { bindingMap :: !(HM.HashMap Cexp [Var]),
    env :: !(IM.IntMap Var),
    clicks :: {-# UNPACK #-} !Int
  }
  deriving (Show, Generic)

rename :: Value -> State CseState Value
rename = zoom #env . gets . flip go
  where
    go env (Var v) | Just v' <- IM.lookup v env = go env (Var v')
    go env (Label v) | Just v' <- IM.lookup v env = go env (Label v')
    go _ v = v

newname :: Var -> Var -> State CseState ()
newname k v = #env %= IM.insert k v

reduce :: Cexp -> (Int, Cexp)
reduce e0 =
  let (e0', s) = runState (go e0) (CseState HM.empty IM.empty 0)
   in (s ^. #clicks, e0')
  where
    go :: Cexp -> State CseState Cexp
    go e@(Fix {}) = embed <$> traverse go (project e)
    go e | not (null (bindings e)) = do
      e' <- traverseOf (typesUsing @Shallow @Value) rename e
      let e'' = common e'
      f <-
        use (#bindingMap % at' e'') >>= \case
          Just wl' ->
            id <$ do
              traverse_ (uncurry newname) $ zip (bindings e') wl'
              #clicks %= (+ 1)
          Nothing -> constr e' <$ (#bindingMap %= HM.insert e'' (bindings e'))
      f . fromJust . foldr (const . Just) Nothing <$> traverse go (project e')
    go e = do
      e' <- traverseOf (typesUsing @Shallow @Value) rename e
      embed <$> traverse go (project e')