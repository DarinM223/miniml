{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

module Miniml.Machine (Condition (..), CMachine (..)) where

import GHC.TypeLits (Nat)
import Miniml.Shared (Access)

data Condition = Neq | Eql | Leq | Geq | Lss | Gtr
  deriving (Show, Eq, Ord)

class CMachine target m | target -> m where
  -- | The number of general purpose registers.
  type N target :: Nat

  -- | The number of callee save registers.
  type K target :: Nat

  -- | Effective Address, assumed to be register, immediate label, or immediate integer.
  type EA target

  -- | Generate a new immediate integer as an operand.
  immed :: Int -> EA target

  -- | Register that holds the current exception handler.
  exnPtrReg :: EA target

  storePtrReg :: EA target

  arithTempReg :: EA target
  globalVarReg :: Maybe (EA target)

  -- | General purpose register 1; when calling an
  -- escaping function, the closure argument is put here.
  standardClosureReg :: EA target

  -- | General purpose register 2; when calling an
  -- escaping function, the source-language argument is put here
  -- (ML languages only have a single argument for functions).
  standardArgReg :: EA target

  standardContReg :: EA target
  miscRegs :: [EA target]

  -- | Generates a new label.
  newLabel :: m (EA target)

  -- | Generates enough zero-filled bytes to get to an "aligned" address.
  align :: m ()

  -- | Generates an embedded descriptor, so if the garbage collector
  -- finds a pointer to the immediately following address it will
  -- be able to find the beginning of the macahine code for this compilation
  -- unit. The descriptor contains an integer offset from this address
  -- to the beginning of the compilation unit.
  mark :: m ()

  -- | Generate a literal integer i in the machine program.
  emitLong :: Int -> m ()

  -- | Puts the ASCII representation of a floating point literal
  -- into the machine-code program.
  realConst :: String -> m ()

  -- | Puts the characters in the string into the machine-code program.
  emitString :: String -> m ()

  -- | Occuring at an address l1 in a program, emits the integer
  -- i + l2 - l1 where i and l2 are the arguments.
  -- Thus it emits the difference (adjusted by a constant) between
  -- the current address and some other specified address.
  emitLabel :: Int -> EA target -> m ()

  -- | Associates the current point in the machine-language program
  -- with an assembly-language label.
  defineLabel :: EA target -> m ()

  -- | Ensures that at least n bytes of space remain in the allocation
  -- region; if not, give control to the runtime system. This instruction
  -- must occur at a safe point, with a mark immediately preceding, and
  -- a register mask right before that. The register mask is typically one
  -- 32-bit word in which a run is set for each register containing a live pointer.
  checkLimit :: Int -> m ()

  -- | jump(x): Jumps to location x, where x can be an immediate label
  -- or the contents of a register.
  jump :: EA target -> m ()

  -- | record(l, r): Allocates a record with fields l and puts a pointer
  -- to it in register r.
  record :: [(EA target, Access)] -> EA target -> m ()

  -- | select(i, v, r): Fetches the ith field of record v, putting
  -- the result in r. r <- v + 4i on a 32 bit byte-addressable machine.
  select :: Int -> EA target -> EA target -> m ()

  -- | fetchIndexB(x, r, z): Indexed 8-bit fetch to get a byte from a string.
  -- r <- M [x + z] (r must not be the same register as x or z)
  fetchIndexB :: EA target -> EA target -> EA target -> m ()

  -- | storeIndexB(x, r, z): Stores a byte into a byte array.
  -- M [x + z] <- r
  storeIndexB :: EA target -> EA target -> EA target -> m ()

  -- | fetchIndexL(x, r, z): Indexed, full-word fetch to get an element
  -- of an array or record. r <- M [x + z] where r is not x or z.
  fetchIndexL :: EA target -> EA target -> EA target -> m ()

  -- | storeIndexL(x, r, z): Stores a word into an array.
  -- M [x + z] <- r
  storeIndexL :: EA target -> EA target -> EA target -> m ()

  ashl, ashr, orb, andb, xorb, notb :: EA target -> EA target -> EA target -> m ()
  add, sub, addt, subt :: EA target -> EA target -> EA target -> m ()

  mult, divt :: EA target -> EA target -> m ()

  -- | bbs(x, l): Branch on bit set. Branch to location l if the number x is odd.
  bbs :: EA target -> EA target -> EA target -> m ()

  -- | ibranch(c, x, y, l): Integer comparison: pc <- l if (x <cond> y).
  ibranch :: Condition -> EA target -> EA target -> EA target -> m ()

  mulf, divf, addf, subf :: EA target -> EA target -> EA target -> m ()

  -- | fbranch(c, x, y, l): Floating comparison: pc <- l if (x <cond> y).
  fbranch :: Condition -> EA target -> EA target -> EA target -> m ()

  -- | loadFloat(x, r): Contents of memory addressed by integer register x
  -- are loaded into floating register r.
  loadFloat :: EA target -> EA target -> m ()

  -- | storeFloat(x, r): Contents of floating register r
  -- are stored into memory addressed by integer register x.
  storeFloat :: EA target -> EA target -> m ()