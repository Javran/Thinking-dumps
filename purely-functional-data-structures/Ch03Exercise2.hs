module Ch03Exercise2 where

import Test.Hspec
import Test.QuickCheck
import qualified Data.List as L
import Ch03Leftist hiding (insert, sortByHeap, main)

{-# ANN module "HLint: ignore Redundant do" #-}

-- to define "insert" without using "merge"
-- we just need to expand the definition of "merge"
-- and manually replace some sub-expressions with more efficient ones
-- thanks to referential transparency, the correctness can be preserved.
insert :: Ord a => a -> Heap a -> Heap a
insert x E = singleton x
insert x h2@(T _ y a2 b2) =
    if x <= y
      then
        -- originally this is:
        -- > makeT x E h2
        -- swaping last 2 arguments should make no difference:
        -- > makeT x h2 E
        -- obviously rank h2 >= rank E = 0 always holds
        -- so let's just replace the sub-expression in that branch to here.
        T 1 x h2 E
      else
        makeT y a2 (insert x b2)

sortByHeap :: Ord a => [a] -> [a]
sortByHeap = toAscList . foldr insert empty

main :: IO ()
main = hspec $ do
    describe "Leftist" $ do
      it "can sort elements (using new insert function)" $ do
        property $ \xs -> sortByHeap (xs :: [Int]) == L.sort xs
