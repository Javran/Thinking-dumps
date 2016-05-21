module Ch04Exercise2 where

import Test.Hspec
import Test.QuickCheck

import Data.List hiding (insert)

{-# ANN module "HLint: ignore Redundant do" #-}

insert :: Ord a => a -> [a] -> [a]
insert v [] = [v]
insert v l@(x:xs)
    | v <= x = v : l
    | otherwise = x : insert v xs

insertionSort :: Ord a => [a] -> [a]
insertionSort = foldl' (flip insert) []

main :: IO ()
main = hspec $ do
    describe "insertionSort" $ do
      it "can sort elements" $ do
        property $ \xs -> insertionSort (xs :: [Int]) == sort xs
