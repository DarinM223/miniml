{-# LANGUAGE OrPatterns #-}
module Miniml.Optimization.Hoist where

import Control.Monad.State.Strict (State, evalState, get, modify')
import Data.Foldable (fold, traverse_)
import Data.Functor.Foldable (cata)
import Data.IntSet qualified as IS
import Data.Maybe (fromMaybe)
import Data.Tuple.Extra (fst3, thd3, third3)
import Miniml.Cps (Cexp (..), CexpF (..), Value (..), Var, var)
import Miniml.Shared (Primop (Alength, Slength))
import Optics (filtered, folded, toListOf, (%), (^..), _1)

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
bindings (Fix fl _) = fst3 <$> fl
bindings (Switch _ _; App _ _) = []
bindings (Primop _ _ wl _) = wl

fixFreeVars :: [Var] -> (Var, [Var], Cexp) -> IS.IntSet
fixFreeVars fl (_, vl, e) = freeVars e IS.\\ IS.fromList (fl ++ vl)

freeVars :: Cexp -> IS.IntSet
freeVars = go IS.empty
  where
    free env = IS.fromList . toListOf (var % filtered (`IS.notMember` env))

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

escaping :: Cexp -> IS.IntSet
escaping = flip evalState IS.empty . cata go
  where
    go :: CexpF (State IS.IntSet IS.IntSet) -> State IS.IntSet IS.IntSet
    go (RecordF fl _ e) = do
      escs <- IS.intersection (IS.fromList (fl ^.. folded % _1 % var)) <$> get
      IS.union escs <$> e
    go (AppF _ vl) = IS.intersection (IS.fromList (vl ^.. folded % var)) <$> get
    go e@(FixF fl _) =
      traverse_ (modify' . IS.insert . fst3) fl >> fold <$> sequence e
    go e = fold <$> sequence e

hoist :: IS.IntSet -> Cexp -> Cexp
hoist esc = stop . cata go
  where
    -- Stop hoisting up Fix and return the expression with the Fix attached.
    stop (_, fl, e) = fix fl e
    fix fl e = if null fl then e else Fix fl e
    push f e = fromMaybe (f e) (pushDown (f e) e)

    -- Returns the free variables of the passed up Fixes,
    -- a list of the passed up Fixes, and the rest of the expression.
    go (RecordF vl w (_, fl, e)) = (IS.empty, [], Record vl w (fix fl e))
    go (SelectF i v w (m, fl, e))
      | IS.notMember w m = (m, fl, push (Select i v w) e)
      | otherwise = (IS.empty, [], push (Select i v w) (fix fl e))
    go (OffsetF i v w (m, fl, e))
      | IS.notMember w m = (m, fl, push (Offset i v w) e)
      | otherwise = (IS.empty, [], push (Offset i v w) (fix fl e))
    go (AppF f vl) = (IS.empty, [], App f vl)
    go (FixF fl (m, fl', e)) =
      case pushDown (Fix closures e) e of
        Just e' -> (m, fl', fix known e')
        Nothing -> (free' <> m, closures ++ fl', fix known e)
      where
        free' = free IS.\\ IS.fromList (fmap fst3 closures)
        (free, closures, known) = foldr goFn (IS.empty, [], []) fl
    go (SwitchF v cl) = (IS.empty, [], Switch v (stop <$> cl))
    go (PrimopF op vl wl [(m, fl, e)])
      | not (any (`IS.member` m) wl) = (m, fl, push' prim e)
      | otherwise = (IS.empty, [], push' prim (fix fl e))
      where
        prim = Primop op vl wl . (: [])
        push' = if op `elem` [Alength, Slength] then push else id
    go (PrimopF op vl wl cl) = (IS.empty, [], Primop op vl wl (stop <$> cl))

    goFn (f, vl, (m, fl, e)) (m', closures, known)
      | not (any (`IS.member` m) vl) = update (m' <> m) (fl ++ closures) known e
      | otherwise = update m' closures known (fix fl e)
      where
        update m0 cs ks body
          | IS.member f esc = (m0 <> bodyVars, (f, vl, body) : cs, ks)
          | otherwise = (m0, cs, (f, vl, body) : ks)
          where
            bodyVars = freeVars body IS.\\ IS.fromList vl

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