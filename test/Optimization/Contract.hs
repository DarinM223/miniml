module Optimization.Contract (tests) where

import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Miniml.Cps qualified as Cps
import Miniml.Optimization.Contract (gatherInfo, reduce)
import Miniml.Shared (Primop (Plus, Times))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Beta contraction"
    [ testCase "Constant switch" testConstSwitch,
      testCase "Non-constant switch" testVarSwitch,
      testCase "Simple beta contraction" testSimpleContract,
      testCase "Tests that contraction fails if applied twice" testContractFail,
      testCase "Removes unused arguments" testRemoveArgs,
      testCase
        "Doesn't remove unused arguments if function escapes"
        testNoRemoveArgsEscape
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

testSimpleContract :: IO ()
testSimpleContract = do
  let e =
        Cps.Fix
          [ ( 2,
              [3, 4, 7],
              Cps.Primop
                Times
                [Cps.Var 3, Cps.Var 4]
                [5]
                [ Cps.Primop
                    Plus
                    [Cps.Var 5, Cps.Var 3]
                    [6]
                    [Cps.App (Cps.Var 7) [Cps.Var 6]]
                ]
            )
          ]
          (Cps.App (Cps.Var 2) [Cps.Var 8, Cps.Var 9, Cps.Var 1])
      info = gatherInfo IS.empty e
      (clicks, e') = reduce IM.empty info e
  show info @?= "fromList [(1,Info {specific = NoSpecificInfo, used = 1}),(2,Info {specific = FunctionInfo (F {formalParams = [3,4,7], body = Primop Times [Var 3,Var 4] [5] [Primop Plus [Var 5,Var 3] [6] [App (Var 7) [Var 6]]], calls = 1, reducible = True}), used = 1}),(3,Info {specific = NoSpecificInfo, used = 2}),(4,Info {specific = NoSpecificInfo, used = 1}),(5,Info {specific = NoSpecificInfo, used = 1}),(6,Info {specific = NoSpecificInfo, used = 1}),(7,Info {specific = NoSpecificInfo, used = 1}),(8,Info {specific = NoSpecificInfo, used = 1}),(9,Info {specific = NoSpecificInfo, used = 1})]"
  clicks @?= 1
  e'
    @?= Cps.Primop
      Times
      [Cps.Var 8, Cps.Var 9]
      [5]
      [ Cps.Primop
          Plus
          [Cps.Var 5, Cps.Var 8]
          [6]
          [Cps.App (Cps.Var 1) [Cps.Var 6]]
      ]

testContractFail :: IO ()
testContractFail = do
  let e =
        Cps.Fix
          [ ( 2,
              [3, 4, 7],
              Cps.Primop
                Times
                [Cps.Var 3, Cps.Var 4]
                [5]
                [Cps.App (Cps.Var 7) [Cps.Var 5]]
            ),
            ( 10,
              [11, 12],
              Cps.App (Cps.Var 2) [Cps.Var 11, Cps.Var 11, Cps.Var 12]
            )
          ]
          (Cps.App (Cps.Var 2) [Cps.Var 8, Cps.Var 9, Cps.Var 10])
      info = gatherInfo IS.empty e
      (clicks, e') = reduce IM.empty info e
  clicks @?= 0
  e' @?= e

testRemoveArgs :: IO ()
testRemoveArgs = do
  let e =
        Cps.Fix
          [ ( 2,
              [3, 4, 5, 6, 7],
              Cps.Primop
                Times
                [Cps.Var 3, Cps.Var 5]
                [8]
                [Cps.App (Cps.Var 7) [Cps.Var 8]]
            ),
            (9, [], Cps.App (Cps.Var 2) [])
          ]
          (Cps.App (Cps.Var 2) (Cps.Var <$> [9 .. 13]))
      info = gatherInfo IS.empty e
      (clicks, e') = reduce IM.empty info e
  clicks @?= 1
  e'
    @?= Cps.Fix
      [ ( 2,
          [3, 5, 7],
          Cps.Primop
            Times
            [Cps.Var 3, Cps.Var 5]
            [8]
            [Cps.App (Cps.Var 7) [Cps.Var 8]]
        ),
        (9, [], Cps.App (Cps.Var 2) [])
      ]
      (Cps.App (Cps.Var 2) [Cps.Var 9, Cps.Var 11, Cps.Var 13])

testNoRemoveArgsEscape :: IO ()
testNoRemoveArgsEscape = do
  let e =
        Cps.Fix
          [ ( 2,
              [3, 4, 5, 6, 7],
              Cps.Primop
                Times
                [Cps.Var 3, Cps.Var 5]
                [8]
                [Cps.App (Cps.Var 7) [Cps.Var 8]]
            ),
            (9, [], Cps.App (Cps.Var 2) [Cps.Var 2])
          ]
          (Cps.App (Cps.Var 2) (Cps.Var <$> [9 .. 13]))
      info = gatherInfo IS.empty e
      (clicks, e') = reduce IM.empty info e
  clicks @?= 0
  e' @?= e