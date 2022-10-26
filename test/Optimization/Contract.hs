module Optimization.Contract (tests) where

import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Miniml.Cps qualified as Cps
import Miniml.Optimization.Contract (gatherInfo, reduce)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Beta contraction"
    [ testCase "Constant switch" testConstSwitch,
      testCase "Non-constant switch" testVarSwitch
    ]

testConstSwitch :: IO ()
testConstSwitch = do
  let e =
        Cps.Switch
          (Cps.Int 1)
          [Cps.App (Cps.Var 0) [Cps.Int 2], Cps.App (Cps.Var 0) [Cps.Int 3]]
      info = gatherInfo IS.empty e
      (clicks, e') = reduce IM.empty info e
  show info @?= "fromList [(0,Info {specific = NoSpecificInfo, used = 2})]"
  clicks @?= 1
  e' @?= Cps.App (Cps.Var 0) [Cps.Int 3]

testVarSwitch :: IO ()
testVarSwitch = do
  let e =
        Cps.Switch
          (Cps.Var 1)
          [Cps.App (Cps.Var 0) [Cps.Int 2], Cps.App (Cps.Var 0) [Cps.Int 3]]
      info = gatherInfo IS.empty e
      (clicks, e') = reduce IM.empty info e
  show info @?= "fromList [(0,Info {specific = NoSpecificInfo, used = 2}),(1,Info {specific = NoSpecificInfo, used = 1})]"
  clicks @?= 0
  e' @?= e