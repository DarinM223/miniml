{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Spill where

import Control.Monad.State.Strict (evalState)
import Data.IntSet qualified as IS
import Miniml.Cps (Cexp (App, Primop, Record, Select), Value (Var))
import Miniml.Optimization.Hoist (freeVars)
import Miniml.Shared (Access (Offp, Selp), Primop (Plus))
import Miniml.Spill qualified as Spill
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Register spilling"
    [ testCase "Example in book" testExample
    ]

testExample :: IO ()
testExample = do
  let (a : b : c : d : e : f : g : h : i : j : k : l : m : rest : _) = [1 ..]
      e0 =
        Select
          0
          (Var a)
          b
          ( Select
              1
              (Var a)
              c
              ( Select
                  2
                  (Var a)
                  d
                  ( Select
                      3
                      (Var a)
                      e
                      ( Select
                          4
                          (Var a)
                          f
                          ( Select
                              5
                              (Var a)
                              g
                              ( Primop
                                  Plus
                                  [Var b, Var c]
                                  [h]
                                  [ Primop
                                      Plus
                                      [Var d, Var e]
                                      [i]
                                      [ Primop
                                          Plus
                                          [Var f, Var g]
                                          [j]
                                          [ Primop
                                              Plus
                                              [Var h, Var i]
                                              [k]
                                              [ Primop
                                                  Plus
                                                  [Var k, Var j]
                                                  [l]
                                                  [ App (Var m) [Var l]
                                                  ]
                                              ]
                                          ]
                                      ]
                                  ]
                              )
                          )
                      )
                  )
              )
          )
      u = freeVars e0
      e0' = evalState (Spill.f 5 IS.empty u IS.empty IS.empty IS.empty e0) rest
      (r : s : c' : d' : g' : m' : _) = [rest + 1 ..]
  e0'
    @?= Select
      0
      (Var a)
      b
      ( Select
          1
          (Var a)
          c
          ( Select
              2
              (Var a)
              d
              ( Record
                  [ (Var a, Offp 0),
                    (Var b, Offp 0),
                    (Var c, Offp 0),
                    (Var d, Offp 0),
                    (Var m, Offp 0)
                  ]
                  r
                  ( Select
                      3
                      (Var a)
                      e
                      ( Select
                          4
                          (Var a)
                          f
                          ( Select
                              5
                              (Var a)
                              g
                              ( Record
                                  [ (Var b, Offp 0),
                                    (Var r, Selp 2 (Offp 0)),
                                    (Var r, Selp 3 (Offp 0)),
                                    (Var e, Offp 0),
                                    (Var f, Offp 0),
                                    (Var g, Offp 0),
                                    (Var r, Selp 4 (Offp 0))
                                  ]
                                  s
                                  ( Select
                                      1
                                      (Var s)
                                      c'
                                      ( Primop
                                          Plus
                                          [Var b, Var c']
                                          [h]
                                          [ Select
                                              2
                                              (Var s)
                                              d'
                                              ( Primop
                                                  Plus
                                                  [Var d', Var e]
                                                  [i]
                                                  [ Select
                                                      5
                                                      (Var s)
                                                      g'
                                                      ( Primop
                                                          Plus
                                                          [Var f, Var g']
                                                          [j]
                                                          [ Primop
                                                              Plus
                                                              [Var h, Var i]
                                                              [k]
                                                              [ Primop
                                                                  Plus
                                                                  [Var k, Var j]
                                                                  [l]
                                                                  [ Select
                                                                      6
                                                                      (Var s)
                                                                      m'
                                                                      (App (Var m') [Var l])
                                                                  ]
                                                              ]
                                                          ]
                                                      )
                                                  ]
                                              )
                                          ]
                                      )
                                  )
                              )
                          )
                      )
                  )
              )
          )
      )
