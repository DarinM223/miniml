module Miniml.Closure.Free where

import Data.Foldable (fold, foldMap')
import Data.Functor.Foldable (cata, project)
import Data.IntMap.Strict qualified as IM
import Data.IntSet (intersection, (\\))
import Data.IntSet qualified as IS
import Data.Maybe (fromMaybe)
import Data.Tuple.Extra (fst3)
import Miniml.Cps (Cexp (App, Fix), CexpF (FixF), Value (Label, Var), Var)
import Miniml.Optimization.Hoist (escaping, freeVars)

-- | A map of function f to a set of functions defined
-- by the same FIX as f.
type SameFix = IM.IntMap IS.IntSet

-- | A map of function f to a set of functions
-- that are applied in the body of f.
type CalledMap = IM.IntMap IS.IntSet

-- | The state of each function's free variables and
-- which functions are closures in iteration i.
data Iteration = Iteration
  { -- | The set of free variables for a function f.
    v :: !(IM.IntMap IS.IntSet),
    -- | The set of functions that seem to require a closure.
    c :: !IS.IntSet,
    -- | The number of arguments for a function f.
    a :: !(IM.IntMap Int)
  }
  deriving (Show, Eq)

(!) :: IM.IntMap IS.IntSet -> IM.Key -> IS.IntSet
m ! k = fromMaybe IS.empty $ m IM.!? k

calcFreeVars :: Int -> SameFix -> CalledMap -> Iteration -> Iteration
calcFreeVars maxRegs sameFix called !iteration
  | iteration' == iteration = iteration'
  | otherwise = calcFreeVars maxRegs sameFix called iteration'
  where
    iteration' = runIteration maxRegs sameFix called iteration

runIteration :: Int -> SameFix -> CalledMap -> Iteration -> Iteration
runIteration maxRegs sameFix called (Iteration !v !c !a) = Iteration v' c' a
  where
    v' :: IM.IntMap IS.IntSet
    v' = IM.mapWithKey updateFreeVars v

    updateFreeVars :: Var -> IS.IntSet -> IS.IntSet
    updateFreeVars f free =
      free <> IS.unions [v ! g | g <- IS.elems ((called ! f) \\ c)]

    c', c1', c2' :: IS.IntSet
    c' = c <> c1' <> c2'
    c1' = IS.fromList $ filter tooManyArgs $ IM.keys v
    c2' = IS.fromList $ filter callsSameFixClosure $ IM.keys v

    tooManyArgs :: Var -> Bool
    tooManyArgs f = maxRegs < IS.size (v ! f) + a IM.! f

    -- If f calls any function defined in the same FIX that requires
    -- the closure, we will make f use the closure.
    callsSameFixClosure :: Var -> Bool
    callsSameFixClosure f =
      not $ IS.null $ c `intersection` (called ! f) `intersection` (sameFix ! f)

fnsInSameFix :: Cexp -> SameFix
fnsInSameFix = cata go
  where
    go :: CexpF SameFix -> SameFix
    go (FixF fl rest) = foldl' addFn IM.empty (fst3 <$> fl) <> rest
      where
        fset = IS.fromList $ fst3 <$> fl
        addFn m f = IM.insert f (IS.delete f fset) m
    go e = fold e

fnsCalledInBody :: Cexp -> CalledMap
fnsCalledInBody = go []
  where
    go :: [Var] -> Cexp -> CalledMap
    go stack (App (Label l) vl) = go stack (App (Var l) vl)
    go stack (App (Var v) _) =
      IM.fromList [(parent, IS.singleton v) | parent <- stack]
    go stack (Fix fl e) =
      IM.unionWith
        (<>)
        (IM.unionsWith (<>) ((\(f, _, body) -> go (f : stack) body) <$> fl))
        (go stack e)
    go stack e = IM.unionsWith (<>) $ go stack <$> project e

initIteration :: Cexp -> Iteration
initIteration e0 = Iteration (goFree e0) (escaping e0) (goArgs e0)
  where
    goFree :: Cexp -> IM.IntMap IS.IntSet
    goFree e@(Fix fl _) =
      IM.fromList [(f, freeVars body \\ IS.fromList vl) | (f, vl, body) <- fl]
        <> foldMap' goFree (project e)
    goFree e = foldMap' goFree (project e)

    goArgs :: Cexp -> IM.IntMap Int
    goArgs e@(Fix fl _) =
      IM.fromList [(f, length vl) | (f, vl, _) <- fl]
        <> foldMap' goArgs (project e)
    goArgs e = foldMap' goArgs (project e)
