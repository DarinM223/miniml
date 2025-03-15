{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Closure.Free where

import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Miniml.Closure.Free (Iteration (Iteration), calcFreeVars, fnsCalledInBody, fnsInSameFix, initIteration)
import Miniml.Cps qualified as Cps
import Miniml.Shared (Access (Offp), Primop (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Test closure free variable calculations"
    [ testCase "Example in figure 10.2 in book" testExample
    ]

testExample :: IO ()
testExample = do
  let (h : x : d : w : t1 : f : arg : c : g : y : j : a : k : b : t2 : z : i : m : n : r : q : t : s : p : e' : u : _) = [1 ..]
  let e =
        Cps.Fix
          [ ( h,
              [x, d],
              Cps.Primop
                Times
                [Cps.Var x, Cps.Var w]
                [t1]
                [Cps.App (Cps.Var d) [Cps.Var t1]]
            ),
            ( f,
              [arg, c],
              Cps.Select
                0
                (Cps.Var arg)
                g
                ( Cps.Select
                    1
                    (Cps.Var arg)
                    y
                    ( Cps.Fix
                        [ ( j,
                            [a],
                            Cps.Fix
                              [ ( k,
                                  [b],
                                  Cps.Primop
                                    Plus
                                    [Cps.Var a, Cps.Var b]
                                    [t2]
                                    [Cps.App (Cps.Var c) [Cps.Var t2]]
                                )
                              ]
                              (Cps.App (Cps.Label h) [Cps.Var z, Cps.Var k])
                          )
                        ]
                        (Cps.App (Cps.Var g) [Cps.Var y, Cps.Var j])
                    )
                )
            )
          ]
          ( Cps.Record
              [(Cps.Var f, Offp 0), (Cps.Int 1, Offp 0)]
              i
              ( Cps.Fix
                  [ ( m,
                      [n, r],
                      Cps.Primop
                        Plus
                        [Cps.Var n, Cps.Var t]
                        [q]
                        [Cps.App (Cps.Var r) [Cps.Var q]]
                    ),
                    ( s,
                      [p],
                      Cps.Primop
                        Plus
                        [Cps.Var p, Cps.Var e']
                        [100]
                        [Cps.App (Cps.Label m) [Cps.Var 100, Cps.Var 0]]
                    )
                  ]
                  ( Cps.Record
                      [(Cps.Var m, Offp 0), (Cps.Int 1, Offp 0)]
                      u
                      (Cps.App (Cps.Label f) [Cps.Var u, Cps.Var s])
                  )
              )
          )
  calcFreeVars 10 (fnsInSameFix e) (fnsCalledInBody e) (initIteration e)
    @?= Iteration
      ( IM.fromList
          [ (h, IS.fromList [w]),
            (f, IS.fromList [h, w, z]),
            (j, IS.fromList [h, w, c, z]),
            (k, IS.fromList [c, a]),
            (m, IS.fromList [t]),
            (s, IS.fromList [0, m, e'])
          ]
      )
      (IS.fromList [f, j, k, m, s])
      (IM.fromList [(h, 2), (f, 2), (j, 1), (k, 1), (m, 2), (s, 1)])
