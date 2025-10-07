{-# LANGUAGE AllowAmbiguousTypes #-}

module Miniml.Codegen (codegen) where

import Data.Proxy
import GHC.TypeLits
import Miniml.Cps qualified as Cps
import Miniml.Machine qualified as M

codegen :: forall target m. (M.CMachine target m, Monad m) => [Cps.Function] -> m ()
codegen funs = do
  label <- M.newLabel @target
  M.emitLabel @target 0 label
  where
    k :: Int
    k = fromInteger $ natVal (Proxy @(M.K target))