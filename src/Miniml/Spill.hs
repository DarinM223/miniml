module Miniml.Spill where

import Control.Monad.State.Strict (State)
import Data.Foldable (toList)
import Data.Functor.Foldable (project)
import Data.IntSet qualified as IS
import Data.List (elemIndex, find, sortOn)
import Miniml.Cps (Cexp (..), Value (..), Var, shallowValues, var)
import Miniml.Optimization.Hoist (bindings, freeVars)
import Miniml.Shared (Access (..), fresh)
import Optics (gplate, (%), (%~), (&), (^..))

root :: Cexp -> [Cexp] -> Cexp
root (Record vl w _) = Record vl w . head
root (Select i v w _) = Select i v w . head
root (Offset i v w _) = Offset i v w . head
root (App fn vl) = const (App fn vl)
root (Fix fl _) = Fix fl . head
root (Switch v _) = Switch v
root (Primop op vl wl _) = Primop op vl wl

rename :: Var -> Var -> Var -> Var
rename v0 v0' v = if v == v0 then v0' else v

-- | Returns the level when the variable is first used.
nextUse :: Var -> Cexp -> Int
nextUse v = go 0 . (: [])
  where
    infinity = 1000000
    go level a = checkLevel a []
      where
        -- Check all nodes in the current level.
        -- Go to the next level if none are found in this one.
        checkLevel [] [] = infinity
        checkLevel [] next = go (level + 1) next
        checkLevel (e : es) next =
          case find (== v) (e ^.. shallowValues % var) of
            Just _ -> level
            Nothing -> checkLevel es (toList (project e) ++ next)

nextNDups :: Int -> IS.IntSet -> Cexp -> IS.IntSet
nextNDups 0 _ _ = IS.empty
nextNDups n dups e
  | n > IS.size dups = dups
  | otherwise = IS.fromList . take n . sortOn (`nextUse` e) $ IS.elems dups

-- | Spill function.
-- Note: Each time free variables are calculated or a variable is renamed,
-- a full pass is performed through the rest of the expression. It should be
-- trivial to convert this to do everything in a single pass.
f ::
  -- | Max registers N.
  Int ->
  -- | Results R bound by previous operator.
  IS.IntSet ->
  -- | Uniquely bound variables U.
  IS.IntSet ->
  -- | Duplicate variables D.
  IS.IntSet ->
  -- | Variable that names the current spill record (if any).
  IS.IntSet ->
  -- | Variables contained in current spill record (if any).
  IS.IntSet ->
  Cexp ->
  State Int Cexp
f !n !r !u !d !sc !sv e
  | mustSpill a || mustSpill w = do
      sv' <- fresh
      Record (get <$> IS.elems vBefore) sv'
        <$> f n IS.empty IS.empty d'' vBefore (IS.singleton sv') e
  | IS.size f' >= 1,
    v <- IS.findMin f',
    Just i <- elemIndex v (IS.elems sc) = do
      v' <- fresh
      let u' = IS.intersection u vBefore
          e' = e & gplate @Value % var %~ rename v v'
          (sc', sv') = if IS.size f' > 1 then (sc, sv) else (scAfter, svAfter)
      Select i (Var (IS.findMin sv)) v'
        <$> f n IS.empty u' (IS.insert v' d') sc' sv' e'
  | otherwise = do
      let u' = IS.intersection (u <> w) vAfter
      root e <$> traverse (f n w u' d' scAfter svAfter) c
  where
    f' = a IS.\\ (u <> d')
    mustSpill s = IS.size (s <> IS.intersection u vAfter) > n - IS.size svAfter
    get v
      | IS.member v d' || IS.member v u = (Var v, Offp 0)
      | Just i <- elemIndex v (IS.elems sc) =
          (Var (IS.findMin sv), Selp i (Offp 0))
      | otherwise = (Var v, Offp 0)
    d'' = IS.intersection (u <> d') vBefore
    d' = nextNDups nDup (IS.intersection d vBefore) e
    nDup = n - IS.size sBefore - IS.size (IS.intersection u vBefore <> r)
    scAfter = if noSpillYet then IS.empty else sc
    svAfter = if noSpillYet then IS.empty else sv
    noSpillYet = IS.null (IS.intersection sc vAfter)
    sBefore = if IS.null sc then IS.empty else sv
    vBefore = freeVars e
    vAfter = IS.unions (freeVars <$> c)
    a = IS.fromList $ e ^.. shallowValues % var
    w = IS.fromList $ bindings e
    c = toList $ project e