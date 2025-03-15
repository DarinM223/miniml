module Optimization.Eta (tests) where

import Miniml.Cps qualified as Cps
import Miniml.Optimization.Eta (reduce)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Eta reduction"
    [ testCase "Simple eta reduction" testSimple,
      testCase "Calls reduced function in body of main function" testRecurse
    ]

testSimple :: IO ()
testSimple = do
  let e =
        Cps.Fix
          [(1, [2, 3], Cps.App (Cps.Var 4) [Cps.Var 2, Cps.Var 3])]
          ( Cps.Fix
              [(5, [6, 7], Cps.App (Cps.Var 0) [Cps.Var 1])]
              (Cps.App (Cps.Var 1) [Cps.Int 1, Cps.Var 1])
          )
      (clicks, e') = reduce e
  clicks @?= 1
  e'
    @?= Cps.Fix
      []
      ( Cps.Fix
          [(5, [6, 7], Cps.App (Cps.Var 0) [Cps.Var 4])]
          (Cps.App (Cps.Var 4) [Cps.Int 1, Cps.Var 4])
      )

testRecurse :: IO ()
testRecurse = do
  let e =
        Cps.Fix
          [ (1, [2, 3], Cps.App (Cps.Var 4) [Cps.Var 2, Cps.Var 3]),
            (4, [5, 6], Cps.App (Cps.Var 1) [Cps.Var 5, Cps.Var 6]),
            (7, [8, 9], Cps.App (Cps.Var 1) [Cps.Var 8, Cps.Var 9]),
            (10, [11, 12], Cps.App (Cps.Var 4) [Cps.Var 11, Cps.Var 12])
          ]
          (Cps.App (Cps.Var 7) [Cps.Int 1, Cps.Int 2])
      (clicks, e') = reduce e
  clicks @?= 3
  e'
    @?= Cps.Fix
      [(4, [5, 6], Cps.App (Cps.Var 4) [Cps.Var 5, Cps.Var 6])]
      (Cps.App (Cps.Var 4) [Cps.Int 1, Cps.Int 2])
