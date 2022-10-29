module Optimization.Flatten where

import Control.Monad.State.Strict (evalState)
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Miniml.Cps qualified as Cps
import Miniml.Optimization.Contract qualified as C
import Miniml.Optimization.Flatten (gatherInfo, reduce)
import Miniml.Shared (Access (Offp), Primop (Plus))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Argument flattening"
    [ testCase "Tests normal flattening" testNormalFlatten
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