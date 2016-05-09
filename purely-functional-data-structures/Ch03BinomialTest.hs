module Ch03BinomialTest where

import Test.Hspec
import Test.QuickCheck
import qualified Data.List as L
import Data.Foldable

import Ch03Binomial

{-# ANN module "HLint: ignore Redundant do" #-}

toAscList :: Ord a => Heap a -> [a]
toAscList h = case viewMin h of
    Nothing -> []
    Just (v, h') -> v : toAscList h'

sortByHeap :: Ord a => [a] -> [a]
sortByHeap = toAscList . foldl' (flip insert) empty

main :: IO ()
main = hspec $ do
    describe "Binomial" $ do
      it "can sort elements" $ do
        property $ \xs -> sortByHeap (xs :: [Int]) == L.sort xs
