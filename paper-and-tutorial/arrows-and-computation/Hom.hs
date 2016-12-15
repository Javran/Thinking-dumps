module Hom where

-- homogeneous functions

{-
  "BalTree" is a collection of 2^n elements:

  - "Zero _" contains 1 = 2^0 element
  - "Succ (Zero (Pair _ _))" contains 2 = 2^1 elements

  note that the constructors form a string, which encodes the depth
  of this tree.
-}

data BalTree a = Zero a | Succ (BalTree (Pair a))

type Pair a = (a,a)

data Hom a b = (a -> b) :&: Hom (Pair a) (Pair b)
