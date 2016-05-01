module Ch03Exercise2 where

import Test.Hspec
import Test.QuickCheck
import qualified Data.List as L
import Ch03Leftist hiding (insert, sortByHeap, main)

{-# ANN module "HLint: ignore Redundant do" #-}

insert :: Ord a => a -> Heap a -> Heap a
insert x E = singleton x
insert x h2@(T _ y a2 b2) =
    if x <= y
      then makeT x E (merge E h2)
      else makeT y a2 (merge (singleton x) b2)

sortByHeap :: Ord a => [a] -> [a]
sortByHeap = toAscList . foldr insert empty

main :: IO ()
main = hspec $ do
    describe "Leftist" $ do
      it "can sort elements (using new insert function)" $ do
        property $ \xs -> sortByHeap (xs :: [Int]) == L.sort xs
