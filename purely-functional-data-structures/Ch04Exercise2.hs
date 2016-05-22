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

{-

to get a clue about the time complexity,
let's see what happens when [3,2,1] is applied to "insertionSort":

> insert 3 (insert 2 (insert 1 []))

This expression is suspended, now say we want to take the first element of it.
So an attempt of pattern matching on the result forces partial computation:

* "insert 3 foo3" needs to know what foo3 looks like
* "insert 2 foo2" needs to know what foo2 looks like
* "insert 1 foo1" needs to know what foo1 looks like, then [] is forced, resulting in itself.
  so "foo1 = []"
* so "insert 1 []" returns "1:[]" immediately
* knowing "foo2 = 1:[]", "insert 2 foo2" tests "2 <= 1", then we have:

> insert 2 foo2
> 1 : insert 2 []

(here "insert 2 []" is a thunk which is not yet forced)

so "foo3 = 1:insert 2 []"

* knowing "foo3 = 1 : insert 2 []" is enough to progress:

> insert 3 (1 : insert 2 [])
> 1 : insert 3 : insert 2 []

(again "insert 3 : insert 2 []" is not yet forced)

* despite that the whole sorted list is not yet fully evaluated,
  we now know that the first element of the result is "1"

-}

insertionSort :: Ord a => [a] -> [a]
insertionSort = foldl' (flip insert) []

main :: IO ()
main = hspec $ do
    describe "insertionSort" $ do
      it "can sort elements" $ do
        property $ \xs -> insertionSort (xs :: [Int]) == sort xs
