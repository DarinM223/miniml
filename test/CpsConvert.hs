{-# LANGUAGE OverloadedStrings #-}

module CpsConvert (tests) where

import Control.Monad.Except (Except, runExcept, throwError)
import Control.Monad.Extra (ifM)
import Control.Monad.State.Strict (StateT, evalStateT, runState, state)
import Data.Text qualified as T
import Miniml.Cps qualified as Cps
import Miniml.Lambda qualified as Lambda
import Miniml.Semantics (DValue (..), SemanticsArgs (..))
import Miniml.Semantics qualified as Semantics
import System.Random (StdGen, getStdGen, randomR)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "CPS Conversion"
    [ testCase "Var" testVar
    ]

infinity :: Double
infinity = 1 / 0

type SemanticM = StateT StdGen (Except T.Text)

defaultHandler :: [DValue loc answer SemanticM] -> store -> SemanticM answer
defaultHandler (String s : _) _ = throwError s
defaultHandler _ _ = throwError "Unknown exception"

args :: SemanticsArgs Int answer SemanticM
args =
  SemanticsArgs
    { minInt = minBound @Int,
      maxInt = maxBound @Int,
      minReal = -infinity,
      maxReal = infinity,
      stringToReal = read @Double . T.unpack,
      nextLoc = (+ 1),
      arbitrarily = ifM (state (randomR (False, True))),
      handlerRef = -1,
      overflowExn = String "Overflow exception",
      divExn = String "Division exception"
    }

testVar :: IO ()
testVar = Semantics.withArgs args $ do
  stdGen <- getStdGen
  let e = 0
      ((k, e'), c) = runState (Cps.convert (Lambda.Var e)) 0
      func ((Int i) : _) store = pure i
      semantics = Semantics.cps [e, k] [Int 420, Func func] e'
      result =
        runExcept . flip evalStateT stdGen $
          semantics (Semantics.initialStore c defaultHandler)
  k @?= 1
  show e' @?= "App (Var 1) [Var 0]"
  result @?= Right 420