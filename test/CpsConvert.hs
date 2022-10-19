{-# LANGUAGE OverloadedStrings #-}

module CpsConvert (tests) where

import Control.Monad.State.Strict (runState)
import Miniml.Cps qualified as Cps
import Miniml.Lambda qualified as L
import Miniml.Semantics (DValue (..))
import Miniml.Semantics qualified as Semantics
import Miniml.Shared (Primop (..))
import Semantic (args, defaultHandler, resultInteger, resultString, runSemanticM)
import System.Random (getStdGen)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "CPS Conversion"
    [ testCase "Var" testVar,
      testCase "Primop1" testPrimop1
    ]

testVar :: IO ()
testVar = Semantics.withArgs args $ do
  stdGen <- getStdGen
  let e = 0
      ((k, e'), c) = runState (Cps.convert (L.Var e)) 0
      semantics = Semantics.cps [e, k] [Int 420, Func resultInteger] e'
      store = Semantics.initialStore c defaultHandler
      result = runSemanticM stdGen $ semantics store
  e' @?= Cps.App (Cps.Var k) [Cps.Var e]
  result @?= Right 420

testPrimop1 :: IO ()
testPrimop1 = Semantics.withArgs args $ do
  stdGen <- getStdGen
  -- if ~(1 - 5 + 1) == 3 then "yes" else "no"
  let e =
        L.Switch
          ( L.App
              (L.Prim Ieql)
              ( L.Record
                  [ L.App
                      (L.Prim Negate)
                      ( L.App
                          (L.Prim Plus)
                          ( L.Record
                              [ L.App
                                  (L.Prim Minus)
                                  (L.Record [L.Int 1, L.Int 5]),
                                L.Int 1
                              ]
                          )
                      ),
                    L.Int 3
                  ]
              )
          )
          [L.Constant 0, L.Constant 1]
          [ (L.IntCon 0, L.String "yes"),
            (L.IntCon 1, L.String "no")
          ]
          Nothing
      ((k, e'), c) = runState (Cps.convert e) (-1)
      semantics = Semantics.cps [k] [Func resultString] e'
      store = Semantics.initialStore c defaultHandler
      result = runSemanticM stdGen $ semantics store
  e'
    @?= Cps.Primop
      Minus
      [Cps.Int 1, Cps.Int 5]
      [1]
      [ Cps.Primop
          Plus
          [Cps.Var 1, Cps.Int 1]
          [2]
          [ Cps.Primop
              Negate
              [Cps.Var 2]
              [3]
              [ Cps.Fix
                  [ ( 5,
                      [6],
                      Cps.Fix
                        [(7, [8], Cps.App (Cps.Var 0) [Cps.Var 8])]
                        ( Cps.Switch
                            (Cps.Var 6)
                            [ Cps.App (Cps.Var 7) [Cps.String "yes"],
                              Cps.App (Cps.Var 7) [Cps.String "no"]
                            ]
                        )
                    )
                  ]
                  ( Cps.Primop
                      Ieql
                      [Cps.Var 3, Cps.Int 3]
                      [4]
                      [ Cps.App (Cps.Var 5) [Cps.Int 0],
                        Cps.App (Cps.Var 5) [Cps.Int 1]
                      ]
                  )
              ]
          ]
      ]
  result @?= Right "yes"