module Miniml.Cps where

import qualified Data.Text as T

type Var = T.Text

data Value
  = Var Var
  | Label Var
  | Int Int
  | Real T.Text
  | String T.Text
  deriving (Show, Eq)

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
  | Not
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

data Cexp
  = Record [(Value, Access)] Var Cexp
  | Select Int Value Var Cexp
  | Offset Int Value Var Cexp
  | App Value [Value]
  | Fix [(Var, [Var], Cexp)] Cexp
  | Switch Value [Cexp]
  | Primop Primop [Value] [Var] [Cexp]