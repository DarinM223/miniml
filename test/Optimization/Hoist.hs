module Optimization.Hoist (tests) where

import Miniml.Cps qualified as Cps
import Miniml.Optimization.Hoist (hoist)
import Miniml.Shared (Primop (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Hoisting"
    [ testCase "Simple hoisting up fix" testSimpleFixHoist
    ]

testSimpleFixHoist :: IO ()
testSimpleFixHoist = do
  let e =
        Cps.Primop
          Plus
          [Cps.Int 1, Cps.Int 2]
          [3]
          [ Cps.Primop
              Plus
              [Cps.Var 3, Cps.Int 3]
              [4]
              [ Cps.Fix
                  [ ( 5,
                      [6],
                      Cps.Fix
                        [(7, [8], Cps.App (Cps.Var 0) [Cps.Var 8])]
                        ( Cps.Select
                            1
                            (Cps.Var 6)
                            9
                            ( Cps.Fix
                                [(10, [11], Cps.App (Cps.Var 0) [Cps.Var 9])]
                                ( Cps.Fix
                                    [(12, [13], Cps.App (Cps.Var 0) [Cps.Var 3])]
                                    (Cps.App (Cps.Var 12) [Cps.Int 1])
                                )
                            )
                        )
                    )
                  ]
                  ( Cps.Fix
                      [(14, [15], Cps.App (Cps.Var 0) [Cps.Var 4])]
                      (Cps.App (Cps.Var 14) [Cps.Int 1])
                  )
              ]
          ]
      (_, e') = hoist e
  e'
    @?= Cps.Primop
      Plus
      [Cps.Int 1, Cps.Int 2]
      [3]
      [ Cps.Primop
          Plus
          [Cps.Var 3, Cps.Int 3]
          [4]
          [ Cps.Fix
              [ ( 5,
                  [6],
                  Cps.Select
                    1
                    (Cps.Var 6)
                    9
                    ( Cps.Fix
                        [ (10, [11], Cps.App (Cps.Var 0) [Cps.Var 9]),
                          (12, [13], Cps.App (Cps.Var 0) [Cps.Var 3])
                        ]
                        (Cps.App (Cps.Var 12) [Cps.Int 1])
                    )
                ),
                (7, [8], Cps.App (Cps.Var 0) [Cps.Var 8]),
                (14, [15], Cps.App (Cps.Var 0) [Cps.Var 4])
              ]
              (Cps.App (Cps.Var 14) [Cps.Int 1])
          ]
      ]
