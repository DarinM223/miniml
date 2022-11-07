module Optimization.Uncurry (tests) where

import Control.Monad.State.Strict (evalState)
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Miniml.Cps qualified as Cps
import Miniml.Optimization.Contract qualified as C
import Miniml.Optimization.Uncurry (reduce)
import Miniml.Shared (Primop (Plus))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Uncurrying"
    [ testCase "Simple uncurrying" testSimple,
      testCase "Uncurry two layer currying" testTwoLayer
      -- TODO(DarinM223): show example with escaping
      -- currying function beta expanded.
    ]

testSimple :: IO ()
testSimple = do
  let e =
        -- fun f(x,c) = let fun g(b,k) = k(x+b) in c(g) end
        Cps.Fix
          [ ( 1,
              [2, 3],
              Cps.Fix
                [ ( 4,
                    [5, 6],
                    Cps.Primop
                      Plus
                      [Cps.Var 2, Cps.Var 5]
                      [7]
                      [Cps.App (Cps.Var 6) [Cps.Var 7]]
                  )
                ]
                (Cps.App (Cps.Var 3) [Cps.Var 4])
            )
          ]
          -- f i j
          ( Cps.Fix
              [(8, [9], Cps.App (Cps.Var 9) [Cps.Var 11, Cps.Var 0])]
              (Cps.App (Cps.Var 1) [Cps.Var 10, Cps.Var 8])
          )
      (clicks, e') = evalState (reduce 10 e) 11
  clicks @?= 1
  e'
    @?= Cps.Fix
      [ ( 1,
          [14, 15],
          Cps.Fix
            [ ( 16,
                [12, 13],
                Cps.App (Cps.Var 17) [Cps.Var 14, Cps.Var 12, Cps.Var 13]
              )
            ]
            (Cps.App (Cps.Var 15) [Cps.Var 16])
        ),
        ( 17,
          [2, 5, 6],
          Cps.Primop
            Plus
            [Cps.Var 2, Cps.Var 5]
            [7]
            [Cps.App (Cps.Var 6) [Cps.Var 7]]
        )
      ]
      ( Cps.Fix
          [(8, [9], Cps.App (Cps.Var 9) [Cps.Var 11, Cps.Var 0])]
          (Cps.App (Cps.Var 1) [Cps.Var 10, Cps.Var 8])
      )
  (iterate (snd . (C.reduce IM.empty =<< C.gatherInfo IS.empty)) e' !! 3)
    @?= Cps.Primop
      Plus
      [Cps.Var 10, Cps.Var 11]
      [7]
      [Cps.App (Cps.Var 0) [Cps.Var 7]]

testTwoLayer :: IO ()
testTwoLayer = do
  let e =
        -- fun f(x,c) = let fun g(b,k) = let fun h(d, l) = l(x+b+d) in k(h) in c(g) end
        Cps.Fix
          [ ( 1,
              [2, 3],
              Cps.Fix
                [ ( 4,
                    [5, 6],
                    Cps.Fix
                      [ ( 7,
                          [8, 9],
                          Cps.Primop
                            Plus
                            [Cps.Var 2, Cps.Var 5]
                            [10]
                            [ Cps.Primop
                                Plus
                                [Cps.Var 10, Cps.Var 8]
                                [11]
                                [Cps.App (Cps.Var 9) [Cps.Var 11]]
                            ]
                        )
                      ]
                      (Cps.App (Cps.Var 6) [Cps.Var 7])
                  )
                ]
                (Cps.App (Cps.Var 3) [Cps.Var 4])
            )
          ]
          -- f i j k
          ( Cps.Fix
              [ (12, [13], Cps.App (Cps.Var 13) [Cps.Var 17, Cps.Var 14]),
                (14, [15], Cps.App (Cps.Var 15) [Cps.Var 18, Cps.Var 0])
              ]
              (Cps.App (Cps.Var 1) [Cps.Var 16, Cps.Var 12])
          )
      (clicks, e') = evalState (reduce 10 e) 18
  clicks @?= 2
  e'
    @?= Cps.Fix
      [ ( 1,
          [21, 22],
          Cps.Fix
            [ ( 23,
                [19, 20],
                Cps.App
                  (Cps.Var 24)
                  [Cps.Var 21, Cps.Var 19, Cps.Var 20]
              )
            ]
            (Cps.App (Cps.Var 22) [Cps.Var 23])
        ),
        ( 24,
          [27, 28, 29],
          Cps.Fix
            [ ( 30,
                [25, 26],
                Cps.App
                  (Cps.Var 31)
                  [Cps.Var 27, Cps.Var 28, Cps.Var 25, Cps.Var 26]
              )
            ]
            (Cps.App (Cps.Var 29) [Cps.Var 30])
        ),
        ( 31,
          [2, 5, 8, 9],
          Cps.Primop
            Plus
            [Cps.Var 2, Cps.Var 5]
            [10]
            [ Cps.Primop
                Plus
                [Cps.Var 10, Cps.Var 8]
                [11]
                [Cps.App (Cps.Var 9) [Cps.Var 11]]
            ]
        )
      ]
      ( Cps.Fix
          [ (12, [13], Cps.App (Cps.Var 13) [Cps.Var 17, Cps.Var 14]),
            (14, [15], Cps.App (Cps.Var 15) [Cps.Var 18, Cps.Var 0])
          ]
          (Cps.App (Cps.Var 1) [Cps.Var 16, Cps.Var 12])
      )