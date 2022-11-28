module Miniml.Shared where

import Control.Monad.State.Strict (MonadState (get), modify')
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

data Access = Offp Int | Selp Int Access
  deriving (Show, Eq, Generic)

instance Hashable Access

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
  | Sequals
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
  | Callcc
  | Throw
  deriving (Show, Eq, Generic)

instance Hashable Primop

fresh :: MonadState Int m => m Int
fresh = modify' (+ 1) >> get
{-# INLINE fresh #-}