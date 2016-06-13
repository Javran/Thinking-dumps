module Ch05PairingHeap where

import Data.Foldable

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

mergePairs :: Ord a => [Heap a] -> Heap a
mergePairs [] = E
mergePairs [h] = h
mergePairs (h1:h2:hs) = (h1 `merge` h2) `merge` mergePairs hs

viewMin :: Ord a => Heap a -> Maybe (a, Heap a)
viewMin E = Nothing
viewMin (T x hs) = Just (x, mergePairs hs)

findMin :: Ord a => Heap a -> Maybe a
findMin = fmap fst . viewMin

deleteMin :: Ord a => Heap a -> Maybe (Heap a)
deleteMin = fmap snd . viewMin

toAscList :: Ord a => Heap a -> [a]
toAscList h = case viewMin h of
    Nothing -> []
    Just (a,h') -> a : toAscList h'

-- TODO: we should really define Heap as a typeclass
-- by doing so we will be able to reuse testcases to test all of them.

fromList :: Ord a => [a] -> Heap a
fromList = foldl' (flip insert) empty
