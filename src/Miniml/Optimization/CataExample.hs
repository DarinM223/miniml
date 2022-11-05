{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Miniml.Optimization.CataExample where

-- An example for how to use catamorphisms to
-- recursively gather information and rewrite expressions.

import Control.Monad.State.Strict (State, evalState, execState, modify')
import Data.Functor.Foldable (cata, embed)
import Data.Generics.Product.Types (ChGeneric, Children, typesUsing)
import Data.Kind (Type)
import Miniml.Cps (Cexp (..), CexpF (..), Value (..))
import Miniml.Shared (Primop (Minus, Plus))
import Optics (traverseOf_)

-- | Counts the number of primops in a CPS expression.
countPrimops :: Cexp -> State Int ()
countPrimops = cata count
  where
    count :: CexpF (State Int ()) -> State Int ()
    count e@(PrimopF {}) = modify' (+ 1) >> sequence_ e
    count e = sequence_ e

-- | Rewrites all CPS offset expressions where
-- the offset > 10 into select expressions.
rewriteCexp :: Cexp -> State Int Cexp
rewriteCexp = cata rewrite
  where
    rewrite :: CexpF (State Int Cexp) -> State Int Cexp
    rewrite (OffsetF i v b r) | i > 10 = Select i v b <$> r
    rewrite e = embed <$> sequence e

-- = 5
testCountPrimops :: Int
testCountPrimops =
  flip execState 0 $
    countPrimops $
      Primop
        Plus
        []
        []
        [ Primop
            Minus
            []
            []
            [Primop Plus [] [] [], Primop Minus [] [] []],
          Primop Plus [] [] []
        ]

e1 :: Cexp
e1 =
  Primop
    Plus
    [Int 1, Int 2]
    []
    [ Select
        11
        (Int 0)
        1
        (Offset 12 (Int 1) 2 (Select 5 (Int 2) 3 (Primop Minus [] [] []))),
      Offset
        5
        (Int 0)
        1
        ( Offset
            13
            (Int 1)
            2
            (Offset 1 (Int 2) 3 (Primop Minus [] [] []))
        )
    ]

-- Primop Plus [Int 1, Int 2] []
--   [ Select 11 (Int 0) 1
--     (Select 12 (Int 1) 2 (Select 5 (Int 2) 3 (Primop Minus [] [] [])))
--   , Offset 5 (Int 0) 1
--     (Select 13 (Int 1) 2 (Offset 1 (Int 2) 3 (Primop Minus [] [] [])))
--   ]
testRewriteCexp :: Cexp
testRewriteCexp = evalState (rewriteCexp e1) 0

-- | Doesn't traverse any Cexp children in type.
data Shallow

type instance Children Shallow a = ChildrenNoRecurseCexp a

type family Filter (t :: Type) (ts :: [Type]) where
  Filter t (t ': rest) = Filter t rest
  Filter t (t' ': rest) = t' ': Filter t rest
  Filter _ '[] = '[]

type family ChildrenNoRecurseCexp (a :: Type) where
  ChildrenNoRecurseCexp a = Filter Cexp (Children ChGeneric a)

-- Prints `Int 1` and `Int 2`.
testShallow :: IO ()
testShallow = traverseOf_ (typesUsing @Shallow @Value) print e1