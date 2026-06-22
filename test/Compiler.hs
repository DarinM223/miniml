{-# LANGUAGE OverloadedStrings #-}

module Compiler (tests) where

import Miniml.Compiler qualified as Compiler
import Miniml.Cps
import Miniml.Lambda qualified as L
import Miniml.Shared (Primop (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Compiler"
    [ testCase "Example 1" testExample1
    ]

testExample1 :: IO ()
testExample1 = do
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
  let (k, e') = Compiler.compile e
  k @?= 0
  -- k "yes"
  e' @?= Fix [] (Select 0 (Var 0) 9 (App (Var 9) [Var 0, String "yes"]))