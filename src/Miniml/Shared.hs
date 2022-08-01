module Miniml.Shared where

import Control.Monad.State.Strict (MonadState (get), modify')

data Access = Offp Int | Selp Int Access
  deriving (Show, Eq)

data Primop
  = Times
  | Plus
  | Minus
  | Div
  | Negate -- ?
  | Ieql
  | Ineq
  | Lt
  | Leq
  | Gt
  | Geq
  | Rangechk
  | Bang -- !
  | Subscript
  | Ordof
  | Assign
  | UnboxedAssign
  | Update
  | UnboxedUpdate
  | Store
  | Makeref
  | MakerefUnboxed
  | Alength
  | Slength
  | Gethdlr
  | Sethdlr
  | Boxed
  | Fadd
  | Fsub
  | Fdiv
  | Fmul
  | Feql
  | Fneq
  | Fge
  | Fgt
  | Fle
  | Flt
  | Rshift
  | Lshift
  | Orb
  | Andb
  | Xorb
  | Notb
  deriving (Show)

fresh :: MonadState Int m => m Int
fresh = modify' (+ 1) >> get
{-# INLINE fresh #-}