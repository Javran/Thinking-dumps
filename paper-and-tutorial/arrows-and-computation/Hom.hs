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

{-
  seems we can expand it indefinitely:

  Hom a b
  <=> (a -> b) :&: Hom (Pair a) (Pair b)
  <=> (a -> b) :&: (Pair a -> Pair b) :&: Hom (Pair (Pair a)) (Pair (Pair b))
  ...

-}
data Hom a b = (a -> b) :&: Hom (Pair a) (Pair b)

-- I have to say this is a smart way of keeping track
apply :: Hom a b -> BalTree a -> BalTree b
apply (f :&: fs) t = case t of
    Zero x -> Zero (f x)
    Succ t' -> Succ (apply fs t')
