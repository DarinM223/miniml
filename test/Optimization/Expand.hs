module Optimization.Expand where

import Control.Monad.State.Strict (evalState, runState)
import Miniml.Cps qualified as Cps
import Miniml.Optimization.Expand (expand, gatherInfo)
import Miniml.Semantics qualified as Semantics
import Miniml.Shared (Primop (Plus), fresh)
import Semantic (args, defaultHandler, resultInteger, runSemanticM)
import System.Random (getStdGen)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?), (@?=))

tests :: TestTree
tests =
  testGroup
    "Beta expansion"
    [ testCase "Simple expansion" testSimple,
      testCase "Doesn't expand large body" testLargeBody,
      testCase "Test expansion has same semantics" testExpandSemantics,
      testCase "Infinite loop terminates" testInfiniteLoop
    ]

testSimple :: IO ()
testSimple = do
  let e =
        Cps.Fix
          [ ( 1,
              [2, 3],
              Cps.Primop
                Plus
                [Cps.Var 2, Cps.Var 3]
                [4]
                [Cps.App (Cps.Var 0) [Cps.Var 4]]
            )
          ]
          (Cps.App (Cps.Var 1) [Cps.Var 5, Cps.Var 6])
      (clicks, e') = evalState (expand 0 (gatherInfo e) e) 6
  clicks @?= 1
  e'
    @?= Cps.Fix
      [ ( 1,
          [2, 3],
          Cps.Primop
            Plus
            [Cps.Var 2, Cps.Var 3]
            [4]
            [Cps.App (Cps.Var 0) [Cps.Var 4]]
        )
      ]
      ( Cps.Primop
          Plus
          [Cps.Var 5, Cps.Var 6]
          [7]
          [Cps.App (Cps.Var 0) [Cps.Var 7]]
      )

mkOp :: Int -> Cps.Var -> Cps.Var -> Cps.Cexp
mkOp n0 v0 k = evalState (go n0 v0) v0
  where
    go 0 v = pure $ Cps.App (Cps.Var k) [Cps.Var v]
    go n v = do
      w <- fresh
      Cps.Primop Plus [Cps.Var v, Cps.Int 1] [w] . (: []) <$> go (n - 1) w

testLargeBody :: IO ()
testLargeBody = do
  let e =
        Cps.Fix
          [ ( 1,
              [2, 3, 4],
              Cps.Primop Plus [Cps.Var 2, Cps.Var 3] [5] [mkOp 40 5 4]
            ),
            (9, [10], Cps.App (Cps.Var 1) [Cps.Var 10, Cps.Var 8, Cps.Var 0])
          ]
          (Cps.App (Cps.Var 1) [Cps.Var 1000, Cps.Var 1001, Cps.Var 9])
  evalState (expand 0 (gatherInfo e) e) 1001 @?= (0, e)

testExpandSemantics :: IO ()
testExpandSemantics = Semantics.withArgs args $ do
  stdGen <- getStdGen
  let e =
        Cps.Fix
          [ ( 1,
              [2, 3, 4],
              Cps.Primop Plus [Cps.Var 2, Cps.Var 3] [5] [mkOp 30 5 4]
            ),
            (9, [10], Cps.App (Cps.Var 1) [Cps.Var 10, Cps.Int 4, Cps.Var 0])
          ]
          (Cps.App (Cps.Var 1) [Cps.Int 2, Cps.Int 3, Cps.Var 9])
      ((clicks, e'), c) = runState (expand 0 (gatherInfo e) e) 1001
      semantics = Semantics.cps [0] [Semantics.mkFunc resultInteger]
      store = Semantics.initialStore c defaultHandler
      result = runSemanticM stdGen $ semantics e store
      result' = runSemanticM stdGen $ semantics e' store
  clicks @?= 2
  e /= e' @? "Function didn't get expanded"
  result @?= Right 69
  result' @?= result

testInfiniteLoop :: IO ()
testInfiniteLoop = do
  let e =
        Cps.Fix
          [ ( 1,
              [2],
              Cps.Primop
                Plus
                [Cps.Var 2, Cps.Int 1]
                [3]
                [Cps.App (Cps.Var 1) [Cps.Var 3]]
            )
          ]
          (Cps.App (Cps.Var 1) [Cps.Int 1])
      (clicks, e') = evalState (expand 0 (gatherInfo e) e) 3
  clicks @?= 6
  e'
    @?= Cps.Fix
      [ ( 1,
          [2],
          Cps.Primop
            Plus
            [Cps.Var 2, Cps.Int 1]
            [3]
            [ Cps.Primop
                Plus
                [Cps.Var 3, Cps.Int 1]
                [4]
                [ Cps.Primop
                    Plus
                    [Cps.Var 4, Cps.Int 1]
                    [5]
                    [ Cps.Primop
                        Plus
                        [Cps.Var 5, Cps.Int 1]
                        [6]
                        [Cps.App (Cps.Var 1) [Cps.Var 6]]
                    ]
                ]
            ]
        )
      ]
      ( Cps.Primop
          Plus
          [Cps.Int 1, Cps.Int 1]
          [7]
          [ Cps.Primop
              Plus
              [Cps.Var 7, Cps.Int 1]
              [8]
              [ Cps.Primop
                  Plus
                  [Cps.Var 8, Cps.Int 1]
                  [9]
                  [Cps.App (Cps.Var 1) [Cps.Var 9]]
              ]
          ]
      )
