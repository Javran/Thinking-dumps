module Ch05Exercise4Test where

import Test.Hspec
import Test.QuickCheck
import Data.List (sort)
import Data.Foldable

import Ch05Splay
import Ch05Exercise4

{-# ANN module "HLint: ignore Redundant do" #-}

-- putting things together
insert :: Ord a => a -> Tree a -> Tree a
insert x t = T (smaller x t) x (bigger x t)

-- converting a splay tree into a list
toAscList :: Tree a -> [a]
toAscList t = case viewMin t of
    Nothing -> []
    Just (v,t') -> v : toAscList t'

main :: IO ()
main = hspec $ do
    describe "Splay" $ do
      it "can sort elements" $ do
          property $ \xs -> sort (xs :: [Int]) == toAscList (foldl' (flip insert) E xs)
