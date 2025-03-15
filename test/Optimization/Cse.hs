{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Optimization.Cse (tests) where

import Miniml.Cps qualified as Cps
import Miniml.Optimization.Cse (reduce)
import Miniml.Shared (Primop (Plus, Times))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Common Subexpression Elimination"
    [ testCase "Works with multiplication example" testTimes,
      testCase "Works across function boundaries" testFnBounds
    ]

testTimes :: IO ()
testTimes = do
  let [k, a, b, c, u, v, w, x, z] = [0 .. 8]
      e =
        Cps.Primop
          Times
          [Cps.Var a, Cps.Var b]
          [u]
          [ Cps.Primop
              Times
              [Cps.Var u, Cps.Var c]
              [v]
              [ Cps.Primop
                  Times
                  [Cps.Var a, Cps.Var b]
                  [w]
                  [ Cps.Primop
                      Times
                      [Cps.Var w, Cps.Var c]
                      [x]
                      [ Cps.Primop
                          Plus
                          [Cps.Var v, Cps.Var x]
                          [z]
                          [Cps.App (Cps.Var k) [Cps.Var z]]
                      ]
                  ]
              ]
          ]
      (clicks, e') = reduce e
  clicks @?= 2
  e'
    @?= Cps.Primop
      Times
      [Cps.Var a, Cps.Var b]
      [u]
      [ Cps.Primop
          Times
          [Cps.Var u, Cps.Var c]
          [v]
          [ Cps.Primop
              Plus
              [Cps.Var v, Cps.Var v]
              [z]
              [Cps.App (Cps.Var k) [Cps.Var z]]
          ]
      ]

testFnBounds :: IO ()
testFnBounds = do
  let e =
        Cps.Select
          1
          (Cps.Var 1)
          2
          ( Cps.Fix
              [ ( 3,
                  [4, 5],
                  Cps.Select
                    1
                    (Cps.Var 1)
                    6
                    (Cps.App (Cps.Var 5) [Cps.Var 6])
                )
              ]
              (Cps.App (Cps.Var 3) [Cps.Int 1, Cps.Int 2])
          )
      (clicks, e') = reduce e
  clicks @?= 1
  e'
    @?= Cps.Select
      1
      (Cps.Var 1)
      2
      ( Cps.Fix
          [(3, [4, 5], Cps.App (Cps.Var 5) [Cps.Var 2])]
          (Cps.App (Cps.Var 3) [Cps.Int 1, Cps.Int 2])
      )
