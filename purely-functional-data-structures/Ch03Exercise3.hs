module Ch03Exercise3 where

import Test.Hspec
import Test.QuickCheck

import Ch03Leftist hiding (main)

{-# ANN module "HLint: ignore Redundant do" #-}

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

-- TODO: prove time complexity?

main :: IO ()
main = hspec $ do
    describe "Leftist" $ do
      it "fromList" $ do
        -- fromList should be the same as "foldr insert empty"
        -- we verify this by using 2 implementations of sortByHeap
        -- and compare whether the same input to them gives the same result.
        property $ \xs -> sortByHeap (xs :: [Int]) == sortByHeap2 xs
