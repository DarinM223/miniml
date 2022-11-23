module Optimization.Hoist (tests) where

import Miniml.Cps qualified as Cps
import Miniml.Optimization.Hoist (hoist, pushDown)
import Miniml.Shared (Primop (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Hoisting"
    [ testCase "Pushes fix down into branches" testFixPushSwitch,
      testCase
        "Doesn't push fix down switch when used in two branches"
        testFixNoPushSwitch,
      testCase
        "If succeeds, then fails trying to continue, return the last success"
        testLastSucceed,
      testCase "Simple hoisting up fix" testSimpleFixHoist,
      testCase
        "Hoisting pushes selects and offsets into switches"
        testHoistPushSwitch,
      testCase
        "Hoisting pushes alength and slength primops into branches"
        testHoistPrimopBranch
    ]

fl :: [(Cps.Var, [Cps.Var], Cps.Cexp)]
fl =
  [ (1, [2, 3], Cps.App (Cps.Var 3) [Cps.Int 1]),
    (4, [5], Cps.App (Cps.Var 5) [Cps.Int 2])
  ]

testFixPushSwitch :: IO ()
testFixPushSwitch = do
  let e =
        Cps.Select
          1
          (Cps.Var 6)
          7
          ( Cps.Offset
              2
              (Cps.Var 6)
              8
              ( Cps.Switch
                  (Cps.Var 8)
                  [ Cps.Primop
                      Plus
                      [Cps.Int 1, Cps.Int 2]
                      [9]
                      [Cps.App (Cps.Var 0) [Cps.Var 9]],
                    Cps.Fix
                      [(10, [11], Cps.App (Cps.Var 11) [Cps.Int 3])]
                      ( Cps.Primop
                          Gt
                          [Cps.Int 1, Cps.Int 2]
                          []
                          [ Cps.App (Cps.Var 0) [Cps.Int 1],
                            Cps.Fix
                              [(12, [13], Cps.App (Cps.Var 13) [Cps.Int 4])]
                              (Cps.App (Cps.Var 4) [Cps.Int 1])
                          ]
                      )
                  ]
              )
          )
  pushDown (Cps.Fix fl e) e
    @?= Just
      ( Cps.Select
          1
          (Cps.Var 6)
          7
          ( Cps.Offset
              2
              (Cps.Var 6)
              8
              ( Cps.Switch
                  (Cps.Var 8)
                  [ Cps.Primop
                      Plus
                      [Cps.Int 1, Cps.Int 2]
                      [9]
                      [Cps.App (Cps.Var 0) [Cps.Var 9]],
                    Cps.Primop
                      Gt
                      [Cps.Int 1, Cps.Int 2]
                      []
                      [ Cps.App (Cps.Var 0) [Cps.Int 1],
                        Cps.Fix
                          [ (12, [13], Cps.App (Cps.Var 13) [Cps.Int 4]),
                            (10, [11], Cps.App (Cps.Var 11) [Cps.Int 3]),
                            (1, [2, 3], Cps.App (Cps.Var 3) [Cps.Int 1]),
                            (4, [5], Cps.App (Cps.Var 5) [Cps.Int 2])
                          ]
                          (Cps.App (Cps.Var 4) [Cps.Int 1])
                      ]
                  ]
              )
          )
      )

testFixNoPushSwitch :: IO ()
testFixNoPushSwitch = do
  let e =
        Cps.Select
          1
          (Cps.Var 6)
          7
          ( Cps.Offset
              2
              (Cps.Var 6)
              8
              ( Cps.Switch
                  (Cps.Var 8)
                  [ Cps.Primop
                      Plus
                      [Cps.Int 1, Cps.Int 2]
                      [9]
                      [Cps.App (Cps.Var 1) [Cps.Int 2, Cps.Var 9]],
                    Cps.App (Cps.Var 4) [Cps.Int 1]
                  ]
              )
          )
  pushDown (Cps.Fix fl e) e @?= Nothing

testLastSucceed :: IO ()
testLastSucceed = do
  let e =
        Cps.Select
          1
          (Cps.Var 6)
          7
          ( Cps.Offset
              2
              (Cps.Var 6)
              8
              ( Cps.Switch
                  (Cps.Var 8)
                  [ Cps.App (Cps.Var 0) [Cps.Int 1],
                    Cps.Switch
                      (Cps.Var 8)
                      [ Cps.Primop
                          Plus
                          [Cps.Int 1, Cps.Int 2]
                          [9]
                          [Cps.App (Cps.Var 1) [Cps.Int 2, Cps.Var 9]],
                        Cps.App (Cps.Var 4) [Cps.Int 1]
                      ]
                  ]
              )
          )
  pushDown (Cps.Fix fl e) e
    @?= Just
      ( Cps.Select
          1
          (Cps.Var 6)
          7
          ( Cps.Offset
              2
              (Cps.Var 6)
              8
              ( Cps.Switch
                  (Cps.Var 8)
                  [ Cps.App (Cps.Var 0) [Cps.Int 1],
                    Cps.Fix
                      [ (1, [2, 3], Cps.App (Cps.Var 3) [Cps.Int 1]),
                        (4, [5], Cps.App (Cps.Var 5) [Cps.Int 2])
                      ]
                      ( Cps.Switch
                          (Cps.Var 8)
                          [ Cps.Primop
                              Plus
                              [Cps.Int 1, Cps.Int 2]
                              [9]
                              [ Cps.App
                                  (Cps.Var 1)
                                  [Cps.Int 2, Cps.Var 9]
                              ],
                            Cps.App (Cps.Var 4) [Cps.Int 1]
                          ]
                      )
                  ]
              )
          )
      )

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
                          (12, [13], Cps.App (Cps.Var 0) [Cps.Var 3]),
                          (7, [8], Cps.App (Cps.Var 0) [Cps.Var 8])
                        ]
                        (Cps.App (Cps.Var 12) [Cps.Int 1])
                    )
                ),
                (14, [15], Cps.App (Cps.Var 0) [Cps.Var 4])
              ]
              (Cps.App (Cps.Var 14) [Cps.Int 1])
          ]
      ]

testHoistPushSwitch :: IO ()
testHoistPushSwitch = do
  let e =
        Cps.Select
          1
          (Cps.Var 1)
          2
          ( Cps.Offset
              2
              (Cps.Var 2)
              3
              ( Cps.Switch
                  (Cps.Var 1)
                  [ Cps.Fix
                      [(4, [5], Cps.App (Cps.Var 5) [Cps.Int 1])]
                      (Cps.App (Cps.Var 0) [Cps.Int 2]),
                    Cps.App (Cps.Var 0) [Cps.Var 3]
                  ]
              )
          )
      (_, e') = hoist e
  e'
    @?= Cps.Fix
      [(4, [5], Cps.App (Cps.Var 5) [Cps.Int 1])]
      ( Cps.Switch
          (Cps.Var 1)
          [ Cps.App (Cps.Var 0) [Cps.Int 2],
            Cps.Select
              1
              (Cps.Var 1)
              2
              ( Cps.Offset
                  2
                  (Cps.Var 2)
                  3
                  (Cps.App (Cps.Var 0) [Cps.Var 3])
              )
          ]
      )

testHoistPrimopBranch :: IO ()
testHoistPrimopBranch = do
  let e =
        Cps.Primop
          Alength
          [Cps.Var 1]
          [2]
          [ Cps.Primop
              Slength
              [Cps.Var 2]
              [3]
              [ Cps.Switch
                  (Cps.Var 1)
                  [ Cps.Fix
                      [(4, [5], Cps.App (Cps.Var 5) [Cps.Int 1])]
                      (Cps.App (Cps.Var 0) [Cps.Int 2]),
                    Cps.App (Cps.Var 0) [Cps.Var 3]
                  ]
              ]
          ]
      (_, e') = hoist e
  e'
    @?= Cps.Fix
      [(4, [5], Cps.App (Cps.Var 5) [Cps.Int 1])]
      ( Cps.Switch
          (Cps.Var 1)
          [ Cps.App (Cps.Var 0) [Cps.Int 2],
            Cps.Primop
              Alength
              [Cps.Var 1]
              [2]
              [ Cps.Primop
                  Slength
                  [Cps.Var 2]
                  [3]
                  [Cps.App (Cps.Var 0) [Cps.Var 3]]
              ]
          ]
      )