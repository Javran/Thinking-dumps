module Ch03Exercise10 where

import Test.Hspec
import Test.QuickCheck
import qualified Data.IntSet as IS
import Data.Foldable
import Data.Maybe

import Ch03RedBlack hiding (insert)

{-# ANN module "HLint: ignore Redundant do" #-}

lBalance :: Color -> Tree a -> a -> Tree a -> Tree a
lBalance Black (T Red (T Red a x b) y c) z d = T Red (T Black a x b) y (T Black c z d)
lBalance Black (T Red a x (T Red b y c)) z d = T Red (T Black a x b) y (T Black c z d)
lBalance c l v r = T c l v r

rBalance :: Color -> Tree a -> a -> Tree a -> Tree a
rBalance Black a x (T Red (T Red b y c) z d) = T Red (T Black a x b) y (T Black c z d)
rBalance Black a x (T Red b y (T Red c z d)) = T Red (T Black a x b) y (T Black c z d)
rBalance c l v r = T c l v r

insert :: Ord a => a -> Tree a -> Tree a
insert x s = T Black a y b
  where
    ins E = T Red E x E
    ins s1@(T c1 a1 y1 b1)
        | x < y1 = lBalance c1 (ins a1) y1 b1
        | x > y1 = rBalance c1 a1 y1 (ins b1)
        | otherwise = s1
    (T _ a y b) = ins s

-- TODO: reduce code replication?

fromList :: Ord a => [a] -> Tree a
fromList = foldl' (flip insert) empty

sortByTree :: Ord a => [a] -> [a]
sortByTree = toAscList . fromList

main :: IO ()
main = hspec $ do
    describe "RedBlack" $ do
      it "can sort elements (no duplicate elements)" $ do
        property $ \xs -> sortByTree (xs :: [Int]) ==
                          (IS.toAscList . IS.fromList $ xs)
      it "should satisfy property of black node depth" $ do
        property $ \xs -> isJust . countBlackDepth . fromList $ (xs :: [Int])
      it "should satisfy the color property" $ do
        property $ \xs -> checkColorProperty . fromList $ (xs :: [Int])
