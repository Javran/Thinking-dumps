module Hom where

-- homogeneous functions

-- "BalTree" is a collection of 2^n elements.
data BalTree a = Zero a | Succ (BalTree (Pair a))

type Pair a = (a,a)
