module Miniml.Optimization.Hoist where

import Data.Foldable (foldl')
import Data.Functor.Foldable (cata)
import Data.IntSet qualified as IS
import Data.Maybe (fromMaybe)
import Data.Tuple.Extra (fst3, thd3, third3)
import Miniml.Cps (Cexp (..), CexpF (..), Value (..), Var)
import Miniml.Shared (Primop (Alength, Slength))

constr :: Cexp -> Cexp -> Cexp
constr (Record vl w _) = Record vl w
constr (Select i v w _) = Select i v w
constr (Offset i v w _) = Offset i v w
constr (App f vl) = const (App f vl)
constr (Fix fl _) = Fix fl
constr (Switch v _) = \e -> Switch v [e]
constr (Primop op vl wl _) = \e -> Primop op vl wl [e]

bindings :: Cexp -> [Var]
bindings (Record _ w _) = [w]
bindings (Select _ _ w _) = [w]
bindings (Offset _ _ w _) = [w]
bindings (App _ _) = []
bindings (Fix fl _) = fst3 <$> fl
bindings (Switch _ _) = []
bindings (Primop _ _ wl _) = wl

fixFreeVars :: [Var] -> (Var, [Var], Cexp) -> IS.IntSet
fixFreeVars fl (_, vl, e) = freeVars e IS.\\ IS.fromList (fl ++ vl)

freeVars :: Cexp -> IS.IntSet
freeVars = go IS.empty
  where
    free env (Label l) = free env (Var l)
    free env (Var v) | not (IS.member v env) = IS.singleton v
    free _ _ = IS.empty

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
hoist = cata go
  where
    go :: CexpF (IS.IntSet, Cexp) -> (IS.IntSet, Cexp)
    go (RecordF vl w (_, e)) = (IS.empty, Record vl w e)
    go (SelectF i v w (m, e)) =
      case e of
        Fix fl e' | not (IS.member w m) -> (m, Fix fl (push e'))
        _ -> (IS.empty, push e)
      where
        push e0 = fromMaybe (Select i v w e0) (pushDown (Select i v w e0) e0)
    go (OffsetF i v w (m, e)) =
      case e of
        Fix fl e' | not (IS.member w m) -> (m, Fix fl (push e'))
        _ -> (IS.empty, push e)
      where
        push e0 = fromMaybe (Offset i v w e0) (pushDown (Offset i v w e0) e0)
    go (AppF f vl) = (IS.empty, App f vl)
    -- TODO(DarinM223): first split the known from the closure requiring functions.
    go (FixF fl (m, e)) =
      case e of
        Fix fl'' e' ->
          case pushDown (Fix fl' e) e' of
            Just e'' -> (m, Fix fl'' e'')
            Nothing -> (IS.union free' m, Fix (fl' ++ fl'') e')
        _ ->
          case pushDown (Fix fl' e) e of
            Just e' -> (IS.empty, e')
            Nothing -> (free', Fix fl' e)
      where
        free' = free IS.\\ IS.fromList (fmap fst3 fl')
        (free, fl') = foldr goFn (IS.empty, []) fl
    go (SwitchF v cl)
      | null fl = (m, Switch v cl')
      | otherwise = (m, Fix fl (Switch v cl'))
      where
        (m, fl, cl') = foldr (goBranch []) (IS.empty, [], []) cl
    go (PrimopF op vl wl [(m, e)]) | op `elem` [Alength, Slength] =
      case e of
        Fix fl e' | not (any (`IS.member` m) wl) -> (m, Fix fl (push e'))
        _ -> (IS.empty, push e)
      where
        push e0 =
          fromMaybe (Primop op vl wl [e0]) (pushDown (Primop op vl wl [e0]) e0)
    go (PrimopF op vl wl cl)
      | null fl = (IS.empty, Primop op vl wl cl')
      | otherwise = (m, Fix fl (Primop op vl wl cl'))
      where
        (m, fl, cl') = foldr (goBranch wl) (IS.empty, [], []) cl

    goBranch wl (m, e) (free, fl, result) = case e of
      Fix fl' e'
        | not (any (`IS.member` m) wl) ->
            (IS.union free m, fl' ++ fl, e' : result)
      _ -> (free, fl, e : result)
    goFn (f, vl, (m, e)) (free, result) =
      case e of
        Fix fl' e'
          | not (any (`IS.member` m) vl) ->
              (free' e', (f, vl, e') : fl' ++ result)
        _ -> (free' e, (f, vl, e) : result)
      where
        free' body = IS.unions [free, m, freeVars body] IS.\\ IS.fromList vl

pushDown :: Cexp -> Cexp -> Maybe Cexp
pushDown = go
  where
    go _ (Record {}) = Nothing
    go e (Select i v w rest)
      | v `elem` (Var <$> bindings e) = Nothing
      | otherwise = Select i v w <$> go e rest
    go e (Offset i v w rest)
      | v `elem` (Var <$> bindings e) = Nothing
      | otherwise = Offset i v w <$> go e rest
    go _ (App {}) = Nothing
    go (Fix fl' _) (Fix fl rest) = Just $ fromMaybe fix $ pushDown fix rest
      where
        fix = Fix (fl ++ fl') rest
    go e (Fix fl rest) = pushBranch e reconstruct (rest : fmap thd3 fl)
      where
        reconstruct :: [Cexp] -> Cexp
        reconstruct (rest' : bl) =
          Fix (uncurry (flip (third3 . const)) <$> zip fl bl) rest'
        reconstruct _ = error "No branches in Fix, this can't happen"
    go e (Switch v el) = pushBranch e (Switch v) el
    go e (Primop op vl wl [rest])
      | any (`elem` (Var <$> bindings e)) vl = Nothing
      | otherwise = Primop op vl wl . (: []) <$> go e rest
    go e (Primop op vl wl el) = pushBranch e (Primop op vl wl) el

    pushBranch e0 mkE el
      | length (filter canPushDown ms) == 1 = Just (mkE el')
      | otherwise = Nothing
      where
        el' = uncurry branch <$> zip ms el
        branch m e = if canPushDown m then fromMaybe e' (pushDown e' e) else e
          where
            e' = constr e0 e
        ms = fmap freeVars el
        canPushDown m = any (`IS.member` m) (bindings e0)