{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ViewPatterns #-}

module Miniml.Semantics where

import Control.Exception (Exception, throw)
import Control.Monad.Extra (ifM, (&&^))
import Data.Char (ord)
import Data.Foldable.Extra (allM)
import Data.Text qualified as T
import Miniml.Cps qualified as Cps
import Miniml.Shared qualified as Cps

data Undefined = Undefined deriving (Show)

instance Exception Undefined

data SemanticsArgs loc answer m = SemanticsArgs
  { minInt :: Int,
    maxInt :: Int,
    minReal :: Double,
    maxReal :: Double,
    stringToReal :: T.Text -> Double,
    nextLoc :: loc -> loc,
    arbitrarily :: forall a. m a -> m a -> m a,
    handlerRef :: loc,
    overflowExn :: DValue loc answer m,
    divExn :: DValue loc answer m
  }

type Store loc answer m = (loc, loc -> DValue loc answer m, loc -> Int)

data DValue loc answer m
  = Record [DValue loc answer m] Int
  | Int Int
  | Real Double
  | Func ([DValue loc answer m] -> Store loc answer m -> m answer)
  | String T.Text
  | Bytearray [loc]
  | Array [loc]
  | Uarray [loc]

fetch :: Store loc answer m -> loc -> DValue loc answer m
fetch (_, f, _) = f

upd ::
  Eq loc =>
  Store loc answer m ->
  loc ->
  DValue loc answer m ->
  Store loc answer m
upd (n, f, g) l v = (n, \i -> if i == l then v else f i, g)

fetchi :: Store loc answer m -> loc -> Int
fetchi (_, _, g) = g

updi :: Eq loc => Store loc answer m -> loc -> Int -> Store loc answer m
updi (n, f, g) l v = (n, f, \i -> if i == l then v else g i)

eqlist ::
  (?args :: SemanticsArgs loc answer m, Eq loc, Monad m) =>
  [DValue loc answer m] ->
  [DValue loc answer m] ->
  m Bool
eqlist l1 l2 = allM (uncurry eq) (zip l1 l2)

eq ::
  (?args :: SemanticsArgs loc answer m, Eq loc, Monad m) =>
  DValue loc answer m ->
  DValue loc answer m ->
  m Bool
eq (Record a i) (Record b j) =
  arbitrarily ?args (pure (i == j) &&^ eqlist a b) (pure False)
eq (Int i) (Int j) = pure $ i == j
eq (Real a) (Real b) = arbitrarily ?args (pure (a == b)) (pure False)
eq (String a) (String b) = arbitrarily ?args (pure (a == b)) (pure False)
eq (Bytearray []) (Bytearray []) = pure True
eq (Bytearray (a : _)) (Bytearray (b : _)) = pure $ a == b
eq (Array []) (Array []) = pure True
eq (Array (a : _)) (Array (b : _)) = pure $ a == b
eq (Uarray []) (Uarray []) = pure True
eq (Uarray (a : _)) (Uarray (b : _)) = pure $ a == b
eq (Func _) (Func _) = throw Undefined
eq (Int i) _ = arbitrarily ?args (pure False) (pure (i < 0 || i > 255))
eq _ (Int i) = arbitrarily ?args (pure False) (pure (i < 0 || i > 255))
eq _ _ = pure False

doRaise ::
  (?args :: SemanticsArgs loc answer m) =>
  DValue loc answer m ->
  Store loc answer m ->
  m answer
doRaise exn s = case fetch s (handlerRef ?args) of
  Func f -> f [exn] s
  _ -> error "doRaise: fetching the handler should return Func"

overflow ::
  (?args :: SemanticsArgs loc answer m) =>
  Int ->
  ([DValue loc answer m] -> Store loc answer m -> m answer) ->
  Store loc answer m ->
  m answer
overflow n c
  | n >= minInt ?args && n <= maxInt ?args = c [Int n]
  | otherwise = doRaise $ overflowExn ?args

overflowr ::
  (?args :: SemanticsArgs loc answer m) =>
  Double ->
  ([DValue loc answer m] -> Store loc answer m -> m answer) ->
  Store loc answer m ->
  m answer
overflowr n c
  | n >= minReal ?args && n <= maxReal ?args = c [Real n]
  | otherwise = doRaise $ overflowExn ?args

type Env loc answer m = Cps.Var -> DValue loc answer m

convertValue ::
  (?args :: SemanticsArgs loc answer m) =>
  Env loc answer m ->
  Cps.Value ->
  DValue loc answer m
convertValue _ (Cps.Int i) = Int i
convertValue _ (Cps.Real r) = Real (stringToReal ?args r)
convertValue _ (Cps.String s) = String s
convertValue env (Cps.Var v) = env v
convertValue env (Cps.Label v) = env v

bind :: Eq t => (t -> p) -> t -> p -> t -> p
bind env v d w = if v == w then d else env w

bindn :: Eq t => (t -> p) -> [t] -> [p] -> t -> p
bindn env (v : vl) (d : dl) = bindn (bind env v d) vl dl
bindn env _ _ = env

evalPath :: DValue loc answer m -> Cps.Access -> DValue loc answer m
evalPath x (Cps.Offp 0) = x
evalPath (Record l i) (Cps.Offp j) = Record l (i + j)
evalPath (Record l i) (Cps.Selp j p) = evalPath (l !! (i + j)) p
evalPath _ _ = error "evalPath: invalid pattern"

evalPrim ::
  (Num loc, Eq loc, ?args :: SemanticsArgs loc answer m, Monad m) =>
  Cps.Primop ->
  [DValue loc answer m] ->
  [[DValue loc answer m] -> Store loc answer m -> m answer] ->
  Store loc answer m ->
  m answer
evalPrim Cps.Plus [Int i, Int j] [c] = overflow (i + j) c
evalPrim Cps.Minus [Int i, Int j] [c] = overflow (i - j) c
evalPrim Cps.Times [Int i, Int j] [c] = overflow (i * j) c
evalPrim Cps.Div [Int _, Int 0] _ = doRaise $ divExn ?args
evalPrim Cps.Div [Int i, Int j] [c] = overflow (i `quot` j) c
evalPrim Cps.Negate [Int i] [c] = overflow (negate i) c
evalPrim Cps.Lt [Int i, Int j] [t, f] = if i < j then t [] else f []
evalPrim Cps.Leq [Int i, Int j] [t, f] = if j < i then f [] else t []
evalPrim Cps.Gt [Int i, Int j] [t, f] = if j < i then t [] else f []
evalPrim Cps.Geq [Int i, Int j] [t, f] = if i < j then f [] else t []
evalPrim Cps.Ieql [a, b] [t, f] = \s -> ifM (eq a b) (t [] s) (f [] s)
evalPrim Cps.Ineq [a, b] [t, f] = \s -> ifM (eq a b) (f [] s) (t [] s)
evalPrim Cps.Rangechk [Int i, Int j] [t, f]
  | j < 0 = if i < 0 then if i < j then t [] else f [] else t []
  | otherwise = if i < 0 then f [] else if i < j then t [] else f []
evalPrim Cps.Boxed [Int i] [t, f] = \s ->
  if i < 0 || i > 255 then arbitrarily ?args (t [] s) (f [] s) else f [] s
evalPrim Cps.Boxed [Record _ _] [t, _] = t []
evalPrim Cps.Boxed [String _] [t, _] = t []
evalPrim Cps.Boxed [Array _] [t, _] = t []
evalPrim Cps.Boxed [Uarray _] [t, _] = t []
evalPrim Cps.Boxed [Bytearray _] [t, _] = t []
evalPrim Cps.Boxed [Func _] [t, _] = t []
evalPrim Cps.Bang [a] [c] = evalPrim Cps.Subscript [a, Int 0] [c]
evalPrim Cps.Subscript [Array a, Int n] [c] = \s -> c [fetch s (a !! n)] s
evalPrim Cps.Subscript [Uarray a, Int n] [c] = \s -> c [Int (fetchi s (a !! n))] s
evalPrim Cps.Subscript [Record a i, Int j] [c] = c [a !! (i + j)]
evalPrim Cps.Ordof [String a, Int i] [c] = c [Int (ord (T.index a i))]
evalPrim Cps.Ordof [Bytearray a, Int i] [c] = \s -> c [Int (fetchi s (a !! i))] s
evalPrim Cps.Assign [a, v] [c] = evalPrim Cps.Update [a, Int 0, v] [c]
evalPrim Cps.Update [Array a, Int n, v] [c] = \s -> c [] (upd s (a !! n) v)
evalPrim Cps.Update [Uarray a, Int n, Int v] [c] = \s -> c [] (updi s (a !! n) v)
evalPrim Cps.UnboxedAssign [a, v] [c] = evalPrim Cps.UnboxedUpdate [a, Int 0, v] [c]
evalPrim Cps.UnboxedUpdate [Array a, Int n, v] [c] = \s -> c [] (upd s (a !! n) v)
evalPrim Cps.UnboxedUpdate [Uarray a, Int n, Int v] [c] =
  \s -> c [] (updi s (a !! n) v)
evalPrim Cps.Store [Bytearray a, Int i, Int v] [c]
  | v < 0 || v >= 256 = throw Undefined
  | otherwise = \s -> c [] (updi s (a !! i) v)
evalPrim Cps.Makeref [v] [c] =
  \(l, f, g) -> c [Array [l]] (upd (nextLoc ?args l, f, g) l v)
evalPrim Cps.MakerefUnboxed [Int v] [c] =
  \(l, f, g) -> c [Uarray [l]] (updi (nextLoc ?args l, f, g) l v)
evalPrim Cps.Alength [Array a] [c] = c [Int (length a)]
evalPrim Cps.Alength [Uarray a] [c] = c [Int (length a)]
evalPrim Cps.Slength [Bytearray a] [c] = c [Int (length a)]
evalPrim Cps.Slength [String a] [c] = c [Int (T.length a)]
evalPrim Cps.Gethdlr [] [c] = \s -> c [fetch s (handlerRef ?args)] s
evalPrim Cps.Sethdlr [h] [c] = \s -> c [] (upd s (handlerRef ?args) h)
evalPrim Cps.Fadd [Real a, Real b] [c] = overflowr (a + b) c
evalPrim Cps.Fsub [Real a, Real b] [c] = overflowr (a - b) c
evalPrim Cps.Fmul [Real a, Real b] [c] = overflowr (a * b) c
evalPrim Cps.Fdiv [Real _, Real 0.0] [_] = doRaise $ divExn ?args
evalPrim Cps.Fdiv [Real a, Real b] [c] = overflowr (a / b) c
evalPrim Cps.Feql [Real a, Real b] [t, f] = if a == b then t [] else f []
evalPrim Cps.Fneq [Real a, Real b] [t, f] = if a == b then f [] else t []
evalPrim Cps.Flt [Real i, Real j] [t, f] = if i < j then t [] else f []
evalPrim Cps.Fle [Real i, Real j] [t, f] = if j < i then f [] else t []
evalPrim Cps.Fgt [Real i, Real j] [t, f] = if j < i then t [] else f []
evalPrim Cps.Fge [Real i, Real j] [t, f] = if i < j then f [] else t []
evalPrim p _ _ = error $ "evalPrim: invalid pattern " ++ show p

denotExpr ::
  (?args :: SemanticsArgs loc answer m, Num loc, Eq loc, Monad m) =>
  (Cps.Var -> DValue loc answer m) ->
  Cps.Cexp ->
  Store loc answer m ->
  m answer
denotExpr env (Cps.Select i (convertValue env -> Record l j) w e) =
  denotExpr (bind env w (l !! (i + j))) e
denotExpr env (Cps.Offset i (convertValue env -> Record l j) w e) =
  denotExpr (bind env w (Record l (i + j))) e
denotExpr env (Cps.App (convertValue env -> Func g) vl) =
  g (fmap (convertValue env) vl)
denotExpr env (Cps.Record vl w e) =
  denotExpr (bind env w (Record (fmap fetchField vl) 0)) e
  where
    fetchField (x, p) = evalPath (convertValue env x) p
denotExpr env (Cps.Switch (convertValue env -> Int i) el) =
  denotExpr env (el !! i)
denotExpr env (Cps.Primop p vl wl el) =
  evalPrim
    p
    (fmap (convertValue env) vl)
    (fmap (\e al -> denotExpr (bindn env wl al) e) el)
denotExpr env (Cps.Fix fl e) = denotExpr (g env) e
  where
    h r1 (_, vl, b) = Func $ \al -> denotExpr (bindn (g r1) vl al) b
    g r = bindn r (fmap (\(n, _, _) -> n) fl) (fmap (h r) fl)
denotExpr _ p = error $ "denotExpr: invalid pattern" ++ show p

type Eval loc answer m =
  [Cps.Var] -> [DValue loc answer m] -> Cps.Cexp -> Store loc answer m -> m answer

env0 :: Env loc answer m
env0 _ = throw Undefined

cps ::
  (?args :: SemanticsArgs loc answer m, Num loc, Eq loc, Monad m) =>
  Eval loc answer m
cps vl dl = denotExpr (bindn env0 vl dl)

withArgs ::
  SemanticsArgs loc answer m -> ((?args :: SemanticsArgs loc answer m) => a) -> a
withArgs args f = let ?args = args in f

initialStore ::
  (Eq loc, ?args :: SemanticsArgs loc answer m) =>
  loc ->
  ([DValue loc answer m] -> Store loc answer m -> m answer) ->
  Store loc answer m
initialStore next handler =
  upd
    (next, \_ -> throw Undefined, \_ -> throw Undefined)
    (handlerRef ?args)
    (Func handler)