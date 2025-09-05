module Miniml.Compiler (maxRegisters, compile) where

import Control.Monad.State.Strict
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
compile e = result
  where
    (result, _c) = flip runState (-1) $ do
      (k, e') <- Cps.convert e
      e'' <- compileRound 0 e'
      let iter =
            Free.calcFreeVars
              10
              (Free.fnsInSameFix e'')
              (Free.fnsCalledInBody e'')
              (Free.initIteration e'')
      e''' <- Lift.lift <$> Closure.convert (Hoist.escaping e'') iter e''
      let u = Hoist.freeVars e'''
      e'''' <- Spill.f maxRegisters IS.empty u IS.empty IS.empty IS.empty e'''
      pure (k, e'''')

compileRound :: Int -> Cps.Cexp -> State Int Cps.Cexp
compileRound r e0 = do
  e' <- constantFolderPass e0
  -- Then hoist, cse, and expand 1 round each.
  let e'' = Hoist.hoist (Hoist.escaping e') e'
  let (clicksCse, e''') = Cse.reduce e''
  let info = Expand.gatherInfo e'''
  (clicksExpand, e'''') <- Expand.expand r info e'''
  if clicksCse + clicksExpand > 0
    then compileRound (r + 1) e''''
    else pure e''''
  where
    constantFolderPass :: Cps.Cexp -> State Int Cps.Cexp
    constantFolderPass e = do
      let info = Contract.gatherInfo IS.empty e
          (clicksContract, e') = Contract.reduce IM.empty info e
      (clicksFlatten, info') <- Flatten.gatherInfo maxRegisters e'
      e'' <- Flatten.reduce info' e'
      let (clicksEtaReduce, e''') = Eta.reduce e''
      (clicksUncurry, e'''') <- Uncurry.reduce maxRegisters e'''
      let totalClicks = clicksContract + clicksFlatten + clicksEtaReduce + clicksUncurry
      if totalClicks > 0
        then constantFolderPass e''''
        else pure e''''