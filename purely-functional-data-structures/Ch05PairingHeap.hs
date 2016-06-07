module Ch05PairingHeap where

data Heap a
  = E
  | T a [Heap a] -- INVARIANT: all elements in this list should be non-empty

empty :: Heap a
empty = E

isEmpty :: Heap a -> Bool
isEmpty E = True
isEmpty _ = False

merge :: Ord a => Heap a -> Heap a -> Heap a
merge h E = h
merge E h = h
merge h1@(T x hs1) h2@(T y hs2) =
    if x <= y
      then T x (h2 : hs1)
      else T y (h1 : hs2)

singleton :: a -> Heap a
singleton v = T v []

{-# ANN insert "HLint: ignore Eta reduce" #-}
insert :: Ord a => a -> Heap a -> Heap a
insert x h = merge (singleton x) h
