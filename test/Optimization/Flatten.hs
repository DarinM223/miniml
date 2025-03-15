module Optimization.Flatten where

import Control.Monad.State.Strict (evalState)
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Miniml.Cps qualified as Cps
import Miniml.Optimization.Contract qualified as C
import Miniml.Optimization.Flatten (fieldExistsAllBranches, gatherInfo, reduce)
import Miniml.Shared (Access (Offp, Selp), Primop (Gt, Plus))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Argument flattening"
    [ testCase "Tests normal flattening" testNormalFlatten,
      testCase
        "Tests escaping fn that doesn't select highest field doesn't flatten"
        testEscapingNoFlatten,
      testCase
        "Tests escaping fn that selects highest field flattens"
        testEscapingFlatten,
      testCase
        "Tests that field that doesn't exist at all branches returns false"
        testFieldExistsAllBranches1,
      testCase
        "Tests that field that exists in all branches returns true"
        testFieldExistsAllBranches2
    ]

testNormalFlatten :: IO ()
testNormalFlatten = do
  let e =
        Cps.Fix
          [ ( 1,
              [2, 3],
              Cps.Select
                0
                (Cps.Var 2)
                4
                ( Cps.Select
                    1
                    (Cps.Var 2)
                    5
                    ( Cps.Select
                        2
                        (Cps.Var 2)
                        6
                        ( Cps.Primop
                            Plus
                            [Cps.Var 4, Cps.Var 5]
                            [7]
                            [ Cps.Primop
                                Plus
                                [Cps.Var 7, Cps.Var 6]
                                [8]
                                [Cps.App (Cps.Var 3) [Cps.Var 8]]
                            ]
                        )
                    )
                )
            )
          ]
          ( Cps.Record
              [(Cps.Var 9, Offp 0), (Cps.Var 10, Offp 0), (Cps.Var 11, Offp 0)]
              12
              (Cps.App (Cps.Var 1) [Cps.Var 12, Cps.Var 0])
          )
      (clicks, info, e') = flip evalState 13 $ do
        (clicks', info') <- gatherInfo 10 e
        e'' <- reduce info' e
        return (clicks', info', e'')
  show info @?= "fromList [(1,FunctionInfo (F {arity = [Count 3 False,Top], alias = Just 14, escape = False})),(2,ArgumentInfo 2),(3,ArgumentInfo (-1)),(12,RecordInfo 3)]"
  clicks @?= 1
  e'
    @?= Cps.Fix
      [ ( 1,
          [18, 19],
          Cps.Select
            0
            (Cps.Var 18)
            20
            ( Cps.Select
                1
                (Cps.Var 18)
                21
                ( Cps.Select
                    2
                    (Cps.Var 18)
                    22
                    ( Cps.App
                        (Cps.Var 14)
                        [Cps.Var 20, Cps.Var 21, Cps.Var 22, Cps.Var 19]
                    )
                )
            )
        ),
        ( 14,
          [15, 16, 17, 3],
          Cps.Record
            [(Cps.Var 15, Offp 0), (Cps.Var 16, Offp 0), (Cps.Var 17, Offp 0)]
            2
            ( Cps.Select
                0
                (Cps.Var 2)
                4
                ( Cps.Select
                    1
                    (Cps.Var 2)
                    5
                    ( Cps.Select
                        2
                        (Cps.Var 2)
                        6
                        ( Cps.Primop
                            Plus
                            [Cps.Var 4, Cps.Var 5]
                            [7]
                            [ Cps.Primop
                                Plus
                                [Cps.Var 7, Cps.Var 6]
                                [8]
                                [Cps.App (Cps.Var 3) [Cps.Var 8]]
                            ]
                        )
                    )
                )
            )
        )
      ]
      ( Cps.Record
          [(Cps.Var 9, Offp 0), (Cps.Var 10, Offp 0), (Cps.Var 11, Offp 0)]
          12
          ( Cps.Select
              0
              (Cps.Var 12)
              23
              ( Cps.Select
                  1
                  (Cps.Var 12)
                  24
                  ( Cps.Select
                      2
                      (Cps.Var 12)
                      25
                      ( Cps.App
                          (Cps.Var 14)
                          [Cps.Var 23, Cps.Var 24, Cps.Var 25, Cps.Var 0]
                      )
                  )
              )
          )
      )
  (iterate (snd . (C.reduce IM.empty =<< C.gatherInfo IS.empty)) e' !! 3)
    @?= Cps.Primop
      Plus
      [Cps.Var 9, Cps.Var 10]
      [7]
      [ Cps.Primop
          Plus
          [Cps.Var 7, Cps.Var 11]
          [8]
          [Cps.App (Cps.Var 0) [Cps.Var 8]]
      ]

testEscapingNoFlatten :: IO ()
testEscapingNoFlatten = do
  let e =
        Cps.Fix
          [ ( 1,
              [2, 3],
              Cps.Select 0 (Cps.Var 2) 4 (Cps.App (Cps.Var 3) [Cps.Var 4])
            ),
            ( 8,
              [9],
              Cps.Record
                [(Cps.Var 10, Offp 0), (Cps.Var 11, Offp 0)]
                12
                (Cps.App (Cps.Var 1) [Cps.Var 12, Cps.Var 0])
            )
          ]
          (Cps.App (Cps.Var 8) [Cps.Var 1])
      (clicks, info, e') = flip evalState 13 $ do
        (clicks', info') <- gatherInfo 10 e
        e'' <- reduce info' e
        return (clicks', info', e'')
  show info @?= "fromList [(1,FunctionInfo (F {arity = [Top,Top], alias = Nothing, escape = True})),(2,ArgumentInfo 0),(3,ArgumentInfo (-1)),(8,FunctionInfo (F {arity = [Top], alias = Nothing, escape = False})),(9,ArgumentInfo (-1)),(12,RecordInfo 2)]"
  clicks @?= 0
  e' @?= e

testEscapingFlatten :: IO ()
testEscapingFlatten = do
  let e =
        Cps.Fix
          [ ( 1,
              [2, 3],
              Cps.Select
                0
                (Cps.Var 2)
                4
                ( Cps.Select
                    1
                    (Cps.Var 2)
                    5
                    ( Cps.Primop
                        Plus
                        [Cps.Var 4, Cps.Var 5]
                        [7]
                        [Cps.App (Cps.Var 3) [Cps.Var 7]]
                    )
                )
            ),
            ( 8,
              [9],
              Cps.Record
                [(Cps.Var 10, Offp 0), (Cps.Var 11, Offp 0)]
                12
                (Cps.App (Cps.Var 1) [Cps.Var 12, Cps.Var 0])
            )
          ]
          (Cps.App (Cps.Var 8) [Cps.Var 1])
      (clicks, info, e') = flip evalState 13 $ do
        (clicks', info') <- gatherInfo 10 e
        e'' <- reduce info' e
        return (clicks', info', e'')
  show info @?= "fromList [(1,FunctionInfo (F {arity = [Count 2 False,Top], alias = Just 14, escape = True})),(2,ArgumentInfo 1),(3,ArgumentInfo (-1)),(8,FunctionInfo (F {arity = [Top], alias = Nothing, escape = False})),(9,ArgumentInfo (-1)),(12,RecordInfo 2)]"
  clicks @?= 1
  e'
    @?= Cps.Fix
      [ ( 1,
          [17, 18],
          Cps.Select
            0
            (Cps.Var 17)
            19
            ( Cps.Select
                1
                (Cps.Var 17)
                20
                (Cps.App (Cps.Var 14) [Cps.Var 19, Cps.Var 20, Cps.Var 18])
            )
        ),
        ( 14,
          [15, 16, 3],
          Cps.Record
            [(Cps.Var 15, Offp 0), (Cps.Var 16, Offp 0)]
            2
            ( Cps.Select
                0
                (Cps.Var 2)
                4
                ( Cps.Select
                    1
                    (Cps.Var 2)
                    5
                    ( Cps.Primop
                        Plus
                        [Cps.Var 4, Cps.Var 5]
                        [7]
                        [Cps.App (Cps.Var 3) [Cps.Var 7]]
                    )
                )
            )
        ),
        ( 8,
          [9],
          Cps.Record
            [(Cps.Var 10, Offp 0), (Cps.Var 11, Offp 0)]
            12
            ( Cps.Select
                0
                (Cps.Var 12)
                21
                ( Cps.Select
                    1
                    (Cps.Var 12)
                    22
                    (Cps.App (Cps.Var 14) [Cps.Var 21, Cps.Var 22, Cps.Var 0])
                )
            )
        )
      ]
      (Cps.App (Cps.Var 8) [Cps.Var 1])

testFieldExistsAllBranches1 :: IO ()
testFieldExistsAllBranches1 = do
  fieldExistsAllBranches 1 5 e @?= False
  fieldExistsAllBranches 1 5 e' @?= False
  where
    e =
      Cps.Primop
        Gt
        [Cps.Var 2, Cps.Int 0]
        []
        [ Cps.Select 5 (Cps.Var 1) 3 (Cps.App (Cps.Var 0) [Cps.Var 3]),
          Cps.App (Cps.Var 0) [Cps.Int 1]
        ]
    e' =
      Cps.Primop
        Plus
        [Cps.Var 2, Cps.Int 3]
        [3]
        [ Cps.Record
            [(Cps.Var 1, Selp 4 (Offp 0))]
            4
            (Cps.App (Cps.Var 0) [Cps.Var 4])
        ]

testFieldExistsAllBranches2 :: IO ()
testFieldExistsAllBranches2 = fieldExistsAllBranches 1 5 e @?= True
  where
    e =
      Cps.Primop
        Gt
        [Cps.Var 2, Cps.Int 0]
        []
        [ Cps.Select 5 (Cps.Var 1) 3 (Cps.App (Cps.Var 0) [Cps.Var 3]),
          Cps.Switch
            (Cps.Var 4)
            [ Cps.Fix
                [(9, [10], Cps.App (Cps.Var 10) [Cps.Int 1])]
                (Cps.Select 5 (Cps.Var 1) 5 (Cps.App (Cps.Var 0) [Cps.Var 5])),
              Cps.Record
                [(Cps.Var 8, Offp 0), (Cps.Var 9, Selp 4 (Offp 0))]
                6
                ( Cps.Record
                    [(Cps.Var 8, Offp 0), (Cps.Var 1, Selp 5 (Offp 0))]
                    7
                    (Cps.App (Cps.Var 0) [Cps.Var 7])
                )
            ]
        ]
