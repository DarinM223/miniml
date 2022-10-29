module Optimization.Contract (tests) where

import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Miniml.Cps qualified as Cps
import Miniml.Optimization.Contract (gatherInfo, reduce)
import Miniml.Shared (Access (Offp, Selp), Primop (..))
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
        testNoRemoveArgsEscape,
      testCase "Record optimizations" testRecordOptimizeSelectPaths,
      testCase "Constant folding arithmetic" testConstFoldArithmetic,
      testCase "Constant folding comparison" testConstFoldCompare,
      testCase "Selection from known record flat" testSelectKnownFlat,
      testCase "Selection from known record nested" testSelectKnownNested,
      testCase "Boolean idiom simplification" testIfIdiom
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
  show info @?= "fromList [(1,Info {specific = NoSpecificInfo, used = 1}),(2,Info {specific = FunctionInfo (F {formalParams = [3,4,7], body = Primop Times [Var 3,Var 4] [5] [Primop Plus [Var 5,Var 3] [6] [App (Var 7) [Var 6]]], calls = 1, reducible = True, specialUse = False}), used = 1}),(3,Info {specific = NoSpecificInfo, used = 2}),(4,Info {specific = NoSpecificInfo, used = 1}),(5,Info {specific = NoSpecificInfo, used = 1}),(6,Info {specific = NoSpecificInfo, used = 1}),(7,Info {specific = NoSpecificInfo, used = 1}),(8,Info {specific = NoSpecificInfo, used = 1}),(9,Info {specific = NoSpecificInfo, used = 1})]"
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

testRecordOptimizeSelectPaths :: IO ()
testRecordOptimizeSelectPaths = do
  let e =
        Cps.Select
          7
          (Cps.Var 1)
          2
          ( Cps.Select
              3
              (Cps.Var 2)
              3
              ( Cps.Record
                  [(Cps.Var 3, Offp 0)]
                  4
                  (Cps.App (Cps.Var 0) [Cps.Var 4])
              )
          )
      e' = iterate (snd . (reduce IM.empty =<< gatherInfo IS.empty)) e !! 3
  e'
    @?= Cps.Record
      [(Cps.Var 1, Selp 7 (Selp 3 (Offp 0)))]
      4
      (Cps.App (Cps.Var 0) [Cps.Var 4])

testConstFoldArithmetic :: IO ()
testConstFoldArithmetic = do
  let e =
        Cps.Primop
          Plus
          [Cps.Int 1, Cps.Int 3]
          [1]
          [ Cps.Primop
              Times
              [Cps.Var 1, Cps.Int 5]
              [2]
              [ Cps.Primop
                  Minus
                  [Cps.Int 2, Cps.Var 2]
                  [3]
                  [ Cps.Primop
                      Negate
                      [Cps.Var 3]
                      [4]
                      [Cps.App (Cps.Var 0) [Cps.Var 4]]
                  ]
              ]
          ]
      info = gatherInfo IS.empty e
      (clicks, e') = reduce IM.empty info e
  clicks @?= 4
  e' @?= Cps.App (Cps.Var 0) [Cps.Int 18]

testConstFoldCompare :: IO ()
testConstFoldCompare = do
  let e =
        Cps.Primop
          Gt
          [Cps.Int 1, Cps.Int 3]
          []
          [ Cps.App (Cps.Var 0) [Cps.Int 1],
            Cps.Primop
              Lt
              [Cps.Int 1, Cps.Int 3]
              []
              [ Cps.Primop
                  Leq
                  [Cps.Int 2, Cps.Int 2]
                  []
                  [ Cps.Primop
                      Geq
                      [Cps.Int 5, Cps.Int 4]
                      []
                      [ Cps.Primop
                          Ieql
                          [Cps.Int 6, Cps.Int 6]
                          []
                          [ Cps.Primop
                              Ineq
                              [Cps.Int 7, Cps.Int 7]
                              []
                              [ Cps.App (Cps.Var 0) [Cps.Int 6],
                                Cps.App (Cps.Var 0) [Cps.Int 7]
                              ],
                            Cps.App (Cps.Var 0) [Cps.Int 5]
                          ],
                        Cps.App (Cps.Var 0) [Cps.Int 4]
                      ],
                    Cps.App (Cps.Var 0) [Cps.Int 3]
                  ],
                Cps.App (Cps.Var 0) [Cps.Int 2]
              ]
          ]
      info = gatherInfo IS.empty e
      (clicks, e') = reduce IM.empty info e
  clicks @?= 6
  e' @?= Cps.App (Cps.Var 0) [Cps.Int 7]

testSelectKnownFlat :: IO ()
testSelectKnownFlat = do
  let e =
        Cps.Primop
          Plus
          [Cps.Var 1, Cps.Var 2]
          [3]
          [ Cps.Record
              [(Cps.Int 1, Offp 0), (Cps.Var 3, Offp 0)]
              4
              ( Cps.Select
                  0
                  (Cps.Var 4)
                  5
                  ( Cps.Select
                      1
                      (Cps.Var 4)
                      6
                      ( Cps.Primop
                          Plus
                          [Cps.Var 5, Cps.Var 6]
                          [7]
                          [Cps.App (Cps.Var 0) [Cps.Var 7]]
                      )
                  )
              )
          ]
      info = gatherInfo IS.empty e
      (clicks, e') = reduce IM.empty info e
  clicks @?= 2
  e'
    @?= Cps.Primop
      Plus
      [Cps.Var 1, Cps.Var 2]
      [3]
      [ Cps.Record
          [(Cps.Int 1, Offp 0), (Cps.Var 3, Offp 0)]
          4
          ( Cps.Primop
              Plus
              [Cps.Int 1, Cps.Var 3]
              [7]
              [Cps.App (Cps.Var 0) [Cps.Var 7]]
          )
      ]

testSelectKnownNested :: IO ()
testSelectKnownNested = do
  let e =
        Cps.Record
          [(Cps.Int 1, Offp 0), (Cps.Var 1, Offp 0)]
          2
          ( Cps.Record
              [(Cps.Var 3, Offp 0), (Cps.Var 2, Offp 0)]
              4
              ( Cps.Record
                  [(Cps.Var 4, Selp 1 (Selp 0 (Offp 0)))]
                  5
                  ( Cps.Select
                      0
                      (Cps.Var 5)
                      6
                      (Cps.App (Cps.Var 0) [Cps.Var 6])
                  )
              )
          )
      info = gatherInfo IS.empty e
      (_, e') = reduce IM.empty info e
  e' @?= Cps.App (Cps.Var 0) [Cps.Int 1]

testIfIdiom :: IO ()
testIfIdiom = do
  let e =
        Cps.Fix
          [ ( 1,
              [2],
              Cps.Primop
                Ieql
                [Cps.Var 2, Cps.Int 0]
                []
                [ Cps.App (Cps.Var 0) [Cps.Int 2],
                  Cps.App (Cps.Var 0) [Cps.Int 1]
                ]
            )
          ]
          ( Cps.Primop
              Gt
              [Cps.Var 3, Cps.Var 4]
              []
              [Cps.App (Cps.Var 1) [Cps.Int 1], Cps.App (Cps.Var 1) [Cps.Int 0]]
          )
      info = gatherInfo IS.empty e
      (_, e') = reduce IM.empty info e
  show info @?= "fromList [(0,Info {specific = NoSpecificInfo, used = 2}),(1,Info {specific = IfIdiomInfo (App (Var 0) [Int 2]) (App (Var 0) [Int 1]), used = 2}),(2,Info {specific = NoSpecificInfo, used = 1}),(3,Info {specific = NoSpecificInfo, used = 1}),(4,Info {specific = NoSpecificInfo, used = 1})]"
  e'
    @?= Cps.Primop
      Gt
      [Cps.Var 3, Cps.Var 4]
      []
      [Cps.App (Cps.Var 0) [Cps.Int 1], Cps.App (Cps.Var 0) [Cps.Int 2]]