module Ch02Exercise1 where

import Test.Hspec
import Test.QuickCheck
import Data.List

{-# ANN module "HLint: ignore Redundant do" #-}

{-

"suffixes" runs in O(n) time because it only traverses
it argument list once.

on each branch of it, only constant number of nodes are created
(either "[[]]" or an new node by using "(:)")

therefore it takes O(n) space for representing the resulting list.

-}

suffixes :: [a] -> [[a]]
suffixes [] = [[]]
suffixes l@(_:xs) = l : suffixes xs

main :: IO ()
main = hspec $ do
    describe "suffixes" $ do
      it "is the same as tails" $ do
        property $ \xs -> tails xs == suffixes (xs :: [Int])
