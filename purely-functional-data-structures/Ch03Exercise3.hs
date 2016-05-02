module Ch03Exercise3 where

import Ch03Leftist hiding (insert, sortByHeap, main)

fromList :: Ord a => [a] -> Heap a
fromList = iter . map singleton
  where
    -- | reduce the list using "mergePairs" until a base case is reached.
    iter [] = empty
    iter [x] = x
    iter xs = iter $ mergePairs xs

    -- | merge every two heaps
    mergePairs :: Ord a => [Heap a] -> [Heap a]
    mergePairs (ha:hb:rs) = merge ha hb : mergePairs rs
    mergePairs xs@[_] = xs
    mergePairs [] = []
