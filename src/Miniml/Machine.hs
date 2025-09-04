{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Miniml.Machine where

import GHC.TypeLits (Nat)
import Miniml.Shared (Access)

class Target target where
  type K target :: Nat

newtype Register = Register {unRegister :: Int} deriving (Show, Eq)

newtype Label = Label {unLabel :: Int} deriving (Show, Eq)

data PseudoOp
  = -- | Generates enough zero-filled bytes to get to an "aligned" address.
    Align
  | Mark
  | -- | Generate a literal integer i in the machine program.
    EmitLong Int
  | -- | Associates the current point in the machine-language program
    -- with an assembly-language label.
    DefineLabel Label
  | -- | Occuring at an address l1 in a program, emits the integer
    -- i + l2 - l1 where i and l2 are the arguments to EmitLabel.
    -- Thus it emits the difference (adjusted by a constant) between
    -- the current address and some other specified address.
    EmitLabel Int Label
  | -- | Puts the characters in the string into the machine-code program.
    EmitString String
  | -- | Puts the ASCII representation of a floating point literal
    -- into the machine-code program.
    RealConstant String

data Operand
  = Reg Register
  | ImmInt Int
  | ImmLabel Label
  deriving (Show, Eq)

data Instruction
  = CheckLimit Int
  | BeginStdFn
  | Jump Operand
  | Record [(Operand, Access)] Register
  | Select Int Operand Register
  | Offset Int Operand Register
  | FetchIndexB Int {- x -} Register {- r -} Int {- z -}
  | StoreIndexB Int {- x -} Register {- r -} Int {- z -}
  | FetchIndexL Int {- x -} Register {- r -} Int {- z -}