module Miniml.Lambda where

import Data.Text (Text)
import Miniml.Shared (Access, Primop)

type Var = Int

data Conrep
  = Undecided
  | Tagged Int
  | Constant Int
  | Transparent
  | TransU
  | TransB
  | Ref
  | Variable Var Access
  | VariableC Var Access

data Con
  = DataCon Conrep
  | IntCon Int
  | RealCon Text
  | StringCon Text

data Lexp
  = Var Var
  | Fn Var Lexp
  | Fix [Var] [Lexp] Lexp
  | App Lexp Lexp
  | Int Int
  | Real Text
  | String Text
  | Switch Lexp [Conrep] [(Con, Lexp)] (Maybe Lexp)
  | Con Conrep Lexp
  | Decon Conrep Lexp
  | Record [Lexp]
  | Select Int Lexp
  | Raise Lexp
  | Handle Lexp Lexp
  | Prim Primop