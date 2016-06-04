{-# LANGUAGE RankNTypes #-}
module Ch05Exercise4Test where

import Test.Hspec
import Test.QuickCheck
import Data.List (sort)
import Data.Foldable

import Ch05Splay
import Ch05Exercise4

{-# ANN module "HLint: ignore Redundant do" #-}

-- putting things together
insertWith :: Ord a =>
          (forall b. Ord b => b -> Tree b -> Tree b) -> a -> Tree a -> Tree a
insertWith smaller x t = T (smaller x t) x (bigger x t)

-- converting a splay tree into a list
toAscList :: Tree a -> [a]
toAscList t = case viewMin t of
    Nothing -> []
    Just (v,t') -> v : toAscList t'

main :: IO ()
main = hspec $ do
    describe "Splay" $ do
      let testSmaller :: (forall a. Ord a => a -> Tree a -> Tree a) -> Property
          testSmaller smaller =
              let insert = insertWith smaller
              in property $ \xs -> sort (xs :: [Int]) == toAscList (foldl' (flip insert) E xs)
      it "can sort elements (smaller1)" $ testSmaller smaller1
      it "can sort elements (smaller2)" $ testSmaller smaller2
