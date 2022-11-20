module Miniml.Optimization.Hoist where

import Data.Foldable (foldl')
import Data.IntSet qualified as IS
import Data.Tuple.Extra (fst3)
import Miniml.Cps (Cexp (..), Value (..), Var)

fixFreeVars :: [Var] -> (Var, [Var], Cexp) -> IS.IntSet
fixFreeVars fl (_, vl, e) = freeVars e IS.\\ IS.fromList (fl ++ vl)

freeVars :: Cexp -> IS.IntSet
freeVars = go IS.empty
  where
    free env (Label l) = free env (Var l)
    free env (Var v) | not (IS.member v env) = IS.singleton v
    free _ _ = IS.empty

    go :: IS.IntSet -> Cexp -> IS.IntSet
    go env (Record vl w e) =
      IS.unions (go (IS.insert w env) e : fmap (free env . fst) vl)
    go env (Select _ v w e) = IS.union (free env v) (go (IS.insert w env) e)
    go env (Offset _ v w e) = IS.union (free env v) (go (IS.insert w env) e)
    go env (App f vl) = IS.unions (fmap (free env) (f : vl))
    go env (Fix fl e) = IS.unions (go env' e : fmap (fixFreeVars fs) fl)
      where
        env' = foldl' (flip IS.insert) env fs
        fs = fst3 <$> fl
    go env (Switch v cl) = IS.unions $ free env v : fmap (go env) cl
    go env (Primop _ vl wl cl) =
      IS.unions $ fmap (free env) vl ++ fmap (go env') cl
      where
        env' = foldl' (flip IS.insert) env wl

-- | Returns the free variables of the passed up Fixes
-- and the updated expression with the hoisted Fixes.
--
-- Only returns the free variables of the fixes because
-- it is only used to check for scope rules when hoisting upward.
hoist :: Cexp -> (IS.IntSet, Cexp)
hoist = go
  where
    go (Record vl w e) = (m, Record vl w e')
      where
        (m, e') = go e
    go (Select i v w e) =
      case e' of
        Fix fl e'' | not (IS.member w m) -> (m, Fix fl (Select i v w e''))
        _ -> (m, Select i v w e')
      where
        (m, e') = go e
    go (Offset i v w e) =
      case e' of
        Fix fl e'' | not (IS.member w m) -> (m, Fix fl (Select i v w e''))
        _ -> (m, Select i v w e')
      where
        (m, e') = go e
    go (App f vl) = (IS.empty, App f vl)
    -- TODO(DarinM223): first split the known from the closure requiring functions.
    go (Fix fl e) =
      case e' of
        Fix fl'' e'' -> (IS.union free m, Fix (fl' ++ fl'') e'')
        _ -> (free, Fix fl' e')
      where
        free = IS.unions (fixFreeVars (fmap fst3 fl') <$> fl')
        fl' = foldr goFn [] fl
        (m, e') = go e
    go (Switch v cl)
      | null fl = (m, Switch v cl')
      | otherwise = (m, Fix fl (Switch v cl'))
      where
        (m, fl, cl') = foldr (goBranch []) (IS.empty, [], []) cl
    go (Primop op vl wl cl)
      | null fl = (m, Primop op vl wl cl')
      | otherwise = (m, Fix fl (Primop op vl wl cl'))
      where
        (m, fl, cl') = foldr (goBranch wl) (IS.empty, [], []) cl

    goBranch wl e (free, fl, result) = case e' of
      Fix fl' e''
        | not (any (`IS.member` m) wl) ->
            (IS.union free m, fl' ++ fl, e'' : result)
      _ -> (free, fl, e' : result)
      where
        (m, e') = go e
    goFn (f, vl, e) result =
      case e' of
        Fix fl' e''
          | not (any (`IS.member` m) vl) -> (f, vl, e'') : fl' ++ result
        _ -> (f, vl, e') : result
      where
        (m, e') = go e

-- pushDown :: Cexp -> Maybe Cexp
-- pushDown = undefined
--   where
--     go (Record vl w e) = (m', Record vl w e')
--       where
--         m' = IS.delete w $ IS.union (freeList (fmap fst vl)) m
--         (m, e') = go e
--     go (Select i v w e) = (m', Select i v w e')
--       where
--         m' = IS.delete w $ maybe m (`IS.insert` m) (var v)
--         (m, e') = go e
--     go (Offset i v w e) = (m', Offset i v w e')
--       where
--         m' = IS.delete w $ maybe m (`IS.insert` m) (var v)
--         (m, e') = go e
--     go (App f vl) = (freeList (f : vl), App f vl)
--     go (Fix fl e) = (m', Fix fl' e')
--       where
--         m' = IS.union free $ m IS.\\ IS.fromList (fmap fst3 fl)
--         (free, fl') = foldr (goFn (fmap fst3 fl)) (IS.empty, []) fl
--         (m, e') = go e
--     go (Switch v cl) = (m', Switch v cl')
--       where
--         m' = maybe m (`IS.insert` m) (var v)
--         (m, cl') = foldr goBranch (IS.empty, []) cl
--     go (Primop op vl wl cl) = (m', Primop op vl wl cl')
--       where
--         m' = IS.union (freeList vl) m IS.\\ IS.fromList wl
--         (m, cl') = foldr goBranch (IS.empty, []) cl

--     goBranch e (free, result) = (IS.union free m, e' : result)
--       where
--         (m, e') = go e
--     goFn fl (f, vl, e) (free, result) = (IS.union free m', (f, vl, e') : result)
--       where
--         m' = m IS.\\ IS.fromList (fl ++ vl)
--         (m, e') = go e