{-# LANGUAGE OverloadedStrings #-}

module CpsConvert (tests) where

import Control.Monad.State.Strict (runState)
import Miniml.Cps qualified as Cps
import Miniml.Lambda qualified as L
import Miniml.Semantics (DValue (..), mkFunc)
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
      testCase "Primop1" testPrimop1,
      testCase "Primop2" testPrimop2,
      testCase "String switch" testStringSwitch
    ]

testVar :: IO ()
testVar = Semantics.withArgs args $ do
  stdGen <- getStdGen
  let e = 0
      ((k, e'), c) = runState (Cps.convert (L.Var e)) 0
      semantics = Semantics.cps [e, k] [Int 420, mkFunc resultInteger] e'
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
          [(L.IntCon 0, L.String "no")]
          (Just (L.String "yes"))
      ((k, e'), c) = runState (Cps.convert e) (-1)
      semantics = Semantics.cps [k] [mkFunc resultString] e'
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
                        [(7, [8], Cps.App (Cps.Var k) [Cps.Var 8])]
                        ( Cps.Primop
                            Ieql
                            [Cps.Var 6, Cps.Int 0]
                            []
                            [ Cps.App (Cps.Var 7) [Cps.String "no"],
                              Cps.App (Cps.Var 7) [Cps.String "yes"]
                            ]
                        )
                    )
                  ]
                  ( Cps.Primop
                      Ieql
                      [Cps.Var 3, Cps.Int 3]
                      [4]
                      [ Cps.App (Cps.Var 5) [Cps.Int 1],
                        Cps.App (Cps.Var 5) [Cps.Int 0]
                      ]
                  )
              ]
          ]
      ]
  result @?= Right "yes"

testPrimop2 :: IO ()
testPrimop2 = Semantics.withArgs args $ do
  stdGen <- getStdGen
  -- 1. -. 5. +. 2. *. 5.
  let e =
        L.Switch
          ( L.App
              (L.Prim Fadd)
              ( L.Record
                  [ L.App
                      (L.Prim Fsub)
                      (L.Record [L.Real "1", L.Real "5"]),
                    L.App
                      (L.Prim Fmul)
                      (L.Record [L.Real "2", L.Real "5"])
                  ]
              )
          )
          []
          [ (L.RealCon "1", L.String "b"),
            (L.RealCon "2", L.String "c"),
            (L.RealCon "3", L.String "d"),
            (L.RealCon "4", L.String "e"),
            (L.RealCon "5", L.String "f"),
            (L.RealCon "6", L.String "g")
          ]
          Nothing
      ((k, e'), c) = runState (Cps.convert e) 0
      semantics = Semantics.cps [k] [mkFunc resultInteger] e'
      store = Semantics.initialStore c defaultHandler
      result = runSemanticM stdGen $ semantics store
  e'
    @?= Cps.Primop
      Fsub
      [Cps.Real "1", Cps.Real "5"]
      [2]
      [ Cps.Primop
          Fmul
          [Cps.Real "2", Cps.Real "5"]
          [3]
          [ Cps.Primop
              Fadd
              [Cps.Var 2, Cps.Var 3]
              [4]
              [ Cps.Fix
                  [(5, [6], Cps.App (Cps.Var k) [Cps.Var 6])]
                  ( Cps.Primop
                      Feql
                      [Cps.Var 4, Cps.Real "6"]
                      []
                      [ Cps.App (Cps.Var 5) [Cps.Int 103],
                        Cps.Primop
                          Feql
                          [Cps.Var 4, Cps.Real "5"]
                          []
                          [ Cps.App (Cps.Var 5) [Cps.Int 102],
                            Cps.Primop
                              Feql
                              [Cps.Var 4, Cps.Real "4"]
                              []
                              [ Cps.App (Cps.Var 5) [Cps.Int 101],
                                Cps.Primop
                                  Feql
                                  [Cps.Var 4, Cps.Real "3"]
                                  []
                                  [ Cps.App (Cps.Var 5) [Cps.Int 100],
                                    Cps.Primop
                                      Feql
                                      [Cps.Var 4, Cps.Real "2"]
                                      []
                                      [ Cps.App (Cps.Var 5) [Cps.Int 99],
                                        Cps.Primop
                                          Feql
                                          [Cps.Var 4, Cps.Real "1"]
                                          []
                                          [ Cps.App (Cps.Var 5) [Cps.Int 98],
                                            Cps.Primop
                                              Gethdlr
                                              []
                                              [7]
                                              [ Cps.App
                                                  (Cps.Var 7)
                                                  [Cps.String "Invalid case"]
                                              ]
                                          ]
                                      ]
                                  ]
                              ]
                          ]
                      ]
                  )
              ]
          ]
      ]
  result @?= Right 103

testStringSwitch :: IO ()
testStringSwitch = Semantics.withArgs args $ do
  stdGen <- getStdGen
  let e =
        L.Switch
          (L.String "hello")
          []
          [ (L.StringCon "aaaaaa", L.String "sarah"),
            (L.StringCon "bbbaaa", L.String "bob"),
            (L.StringCon "aa", L.String "joe"),
            (L.StringCon "bb", L.String "bar"),
            (L.StringCon "aaaa", L.String "foo"),
            (L.StringCon "bbbb", L.String "blah"),
            (L.StringCon "hello", L.String "world"),
            (L.StringCon "world", L.String "hello")
          ]
          (Just (L.String "Default"))
      ((k, e'), c) = runState (Cps.convert e) 0
      semantics = Semantics.cps [k] [mkFunc resultString] e'
      store = Semantics.initialStore c defaultHandler
      result = runSemanticM stdGen $ semantics store
  e'
    @?= Cps.Fix
      [(2, [3], Cps.App (Cps.Var k) [Cps.Var 3])]
      ( Cps.Primop
          Slength
          [Cps.String "hello"]
          [4]
          [ Cps.Primop
              Ieql
              [Cps.Var 4, Cps.Int 6]
              []
              [ Cps.Primop
                  Sequals
                  [Cps.String "hello", Cps.String "bbbaaa"]
                  []
                  [ Cps.App (Cps.Var 2) [Cps.String "bob"],
                    Cps.Primop
                      Sequals
                      [ Cps.String "hello",
                        Cps.String "aaaaaa"
                      ]
                      []
                      [ Cps.App (Cps.Var 2) [Cps.String "sarah"],
                        Cps.App (Cps.Var 2) [Cps.String "Default"]
                      ]
                  ],
                Cps.Primop
                  Ieql
                  [Cps.Var 4, Cps.Int 5]
                  []
                  [ Cps.Primop
                      Sequals
                      [Cps.String "hello", Cps.String "world"]
                      []
                      [ Cps.App (Cps.Var 2) [Cps.String "hello"],
                        Cps.Primop
                          Sequals
                          [Cps.String "hello", Cps.String "hello"]
                          []
                          [ Cps.App (Cps.Var 2) [Cps.String "world"],
                            Cps.App (Cps.Var 2) [Cps.String "Default"]
                          ]
                      ],
                    Cps.Primop
                      Ieql
                      [Cps.Var 4, Cps.Int 4]
                      []
                      [ Cps.Primop
                          Sequals
                          [Cps.String "hello", Cps.String "bbbb"]
                          []
                          [ Cps.App (Cps.Var 2) [Cps.String "blah"],
                            Cps.Primop
                              Sequals
                              [Cps.String "hello", Cps.String "aaaa"]
                              []
                              [ Cps.App (Cps.Var 2) [Cps.String "foo"],
                                Cps.App (Cps.Var 2) [Cps.String "Default"]
                              ]
                          ],
                        Cps.Primop
                          Ieql
                          [Cps.Var 4, Cps.Int 2]
                          []
                          [ Cps.Primop
                              Sequals
                              [Cps.String "hello", Cps.String "bb"]
                              []
                              [ Cps.App (Cps.Var 2) [Cps.String "bar"],
                                Cps.Primop
                                  Sequals
                                  [Cps.String "hello", Cps.String "aa"]
                                  []
                                  [ Cps.App (Cps.Var 2) [Cps.String "joe"],
                                    Cps.App (Cps.Var 2) [Cps.String "Default"]
                                  ]
                              ],
                            Cps.App (Cps.Var 2) [Cps.String "Default"]
                          ]
                      ]
                  ]
              ]
          ]
      )
  result @?= Right "world"
