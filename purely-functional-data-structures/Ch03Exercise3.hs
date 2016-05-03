module Ch03Exercise3 where

import Test.Hspec
import Test.QuickCheck

import Ch03Leftist hiding (main)

{-# ANN module "HLint: ignore Redundant do" #-}

{-

to show it the leftist heap creation takes ceil(log(n)) passes:

let the size of the list be n, after i passes, the number of nodes in the list is:

n / (2^i)

so to merge this list of heaps into just one, we need to find the minimum i that satisfies:

* n / (2^i) <= 1
* n <= 2^i
* log(n) <= i

therefore it requires i = ceil(log(n)) passes

-}
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

sortByHeap2 :: Ord a => [a] -> [a]
sortByHeap2 = toAscList . fromList

{-

prove the time complexity of creating a leftist heap from a list.

let i be the i-th pass, starting from 0: (with c being a constant)

pass | number of elements | element tree size | merge cost
i    | n / 2^i            | 2^i               | O( log(2^i * 2) ) = (i+1)*c

Total cost: sum( (i+1)*c*n/2^i , i = 0..ceil(log(n)) )

we can work out sum( (i+1)/2^i, i = 0..m) for any m,
which is:

sum( (i+1)/2^i, i = 0..m) = 4-(3+m)/2^m

-}
main :: IO ()
main = hspec $ do
    describe "Leftist" $ do
      it "fromList" $ do
        -- fromList should be the same as "foldr insert empty"
        -- we verify this by using 2 implementations of sortByHeap
        -- and compare whether the same input to them gives the same result.
        property $ \xs -> sortByHeap (xs :: [Int]) == sortByHeap2 xs
