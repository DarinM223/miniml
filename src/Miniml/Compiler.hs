module Miniml.Compiler (maxRegisters, compile) where

import Control.Monad.Identity (runIdentityT)
import Control.Monad.State.Strict (State, lift, runState)
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Miniml.Closure.Convert qualified as Closure
import Miniml.Closure.Free qualified as Free
import Miniml.Closure.Lift qualified as Lift
import Miniml.Cps qualified as Cps
import Miniml.Lambda qualified as Lam
import Miniml.Optimization.Contract qualified as Contract
import Miniml.Optimization.Cse qualified as Cse
import Miniml.Optimization.Eta qualified as Eta
import Miniml.Optimization.Expand qualified as Expand
import Miniml.Optimization.Flatten qualified as Flatten
import Miniml.Optimization.Hoist qualified as Hoist
import Miniml.Optimization.Uncurry qualified as Uncurry
import Miniml.Spill qualified as Spill

maxRegisters :: Int
maxRegisters = 10

compile :: Lam.Lexp -> (Cps.Var, Cps.Cexp)
compile _e = result
  where
    (result, _c) = flip runState (-1) $ runIdentityT $ do
      (k, _e) <- lift $ Cps.convert _e
      _e <- lift $ compileRound 0 _e
      let iter =
            Free.calcFreeVars
              10
              (Free.fnsInSameFix _e)
              (Free.fnsCalledInBody _e)
              (Free.initIteration _e)
      _e <- lift $ Lift.lift <$> Closure.convert (Hoist.escaping _e) iter _e
      let u = Hoist.freeVars _e
      _e <- lift $ Spill.f maxRegisters IS.empty u IS.empty IS.empty IS.empty _e
      pure (k, _e)

compileRound :: Int -> Cps.Cexp -> State Int Cps.Cexp
compileRound r _e = runIdentityT $ do
  _e <- lift $ constantFolderPass _e
  -- Then hoist, cse, and expand 1 round each.
  _e <- pure $ Hoist.hoist (Hoist.escaping _e) _e
  (clicksCse, _e) <- pure $ Cse.reduce _e
  let info = Expand.gatherInfo _e
  (clicksExpand, _e) <- lift $ Expand.expand r info _e
  if clicksCse + clicksExpand > 0
    then lift $ compileRound (r + 1) _e
    else pure _e
  where
    constantFolderPass :: Cps.Cexp -> State Int Cps.Cexp
    constantFolderPass _e = runIdentityT $ do
      let info = Contract.gatherInfo IS.empty _e
      (clicksContract, _e) <- pure $ Contract.reduce IM.empty info _e
      (clicksFlatten, info') <- lift $ Flatten.gatherInfo maxRegisters _e
      _e <- lift $ Flatten.reduce info' _e
      (clicksEtaReduce, _e) <- pure $ Eta.reduce _e
      (clicksUncurry, _e) <- lift $ Uncurry.reduce maxRegisters _e
      let totalClicks = clicksContract + clicksFlatten + clicksEtaReduce + clicksUncurry
      if totalClicks > 0
        then lift $ constantFolderPass _e
        else pure _e