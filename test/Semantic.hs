{-# LANGUAGE OverloadedStrings #-}

module Semantic
  ( args,
    defaultHandler,
    resultInteger,
    resultString,
    runSemanticM,
  )
where

import Control.Monad.Except (Except, runExcept, throwError)
import Control.Monad.Extra (ifM)
import Control.Monad.State.Strict (StateT, evalStateT, state)
import Data.Text qualified as T
import Miniml.Semantics (DValue (..), SemanticsArgs (..))
import System.Random (StdGen, randomR)

infinity :: Double
infinity = 1 / 0

type SemanticM = StateT StdGen (Except T.Text)

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

defaultHandler :: [DValue loc answer SemanticM] -> store -> SemanticM answer
defaultHandler (String s : _) _ = throwError s
defaultHandler _ _ = throwError "Unknown exception"

runSemanticM :: StdGen -> SemanticM a -> Either T.Text a
runSemanticM stdGen = runExcept . flip evalStateT stdGen

resultInteger :: [DValue loc Int m] -> store -> SemanticM Int
resultInteger (Int i : _) _ = pure i
resultInteger _ _ = throwError "Not an integer"

resultString :: [DValue loc T.Text m] -> store -> SemanticM T.Text
resultString (String s : _) _ = pure s
resultString _ _ = throwError "Not a string"