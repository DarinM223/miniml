module CpsConvert (tests) where

import Control.Exception (throw)
import Data.Text qualified as T
import Miniml.Cps qualified as Cps
import Miniml.Lambda qualified as Lambda
import Miniml.Semantics
  ( DValue (..),
    SemanticsArgs (..),
    Undefined (Undefined),
    cpsSemantics,
    withArgs,
  )
import System.IO.Unsafe (unsafePerformIO)
import System.Random (randomRIO)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "CPS Conversion"
    [ testCase "Var" testVar
    ]

args :: SemanticsArgs Int answer
args =
  SemanticsArgs
    { minInt = minBound @Int,
      maxInt = maxBound @Int,
      minReal = fromIntegral (minBound @Int),
      maxReal = fromIntegral (maxBound @Int),
      stringToReal = read @Double . T.unpack,
      nextLoc = (+ 1),
      arbitrarily = \(a, b) ->
        if unsafePerformIO (randomRIO (False, True)) then a else b,
      handlerRef = undefined,
      overflowExn = undefined,
      divExn = undefined
    }

testVar :: IO ()
testVar = withArgs args $ do
  let e = 0
      ((k, e'), _) = Cps.runConvertM (Cps.convert (Lambda.Var e)) 0
      func ((Int i) : _) store = i
      semantics = cpsSemantics [e, k] [Int 420, Func func] e'
      result = semantics (0, \_ -> throw Undefined, \_ -> throw Undefined)
  k @?= 1
  show e' @?= "App (Var 1) [Var 0]"
  result @?= 420