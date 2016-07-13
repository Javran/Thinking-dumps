module DisjointSetTest where

import Test.Hspec
import Test.QuickCheck
import Data.Foldable
import Control.Monad

import qualified DisjointSet as DS
import qualified Data.IntSet as IS

{-# ANN module "HLint: ignore Redundant do" #-}

main :: IO ()
main = hspec $ do
    describe "insert & fromList" $ do
        specify "basic property of union-ing & querying" $ do
            {-
              we can simulate a set using disjoint set:
              * first we pick "0" as a "base element" (which should not
                 conflict with value domain)
              * when we want to insert something into the set, we simply union it
                with the base element

              the idea above in mind, here we have a simple test: simulate a set
              and see if that works.
            -}
            property $ do
                xs1 <- replicateM 50 $ choose (1,500 :: Int)
                let s1 = IS.fromList xs1
                    s2 = foldl' update DS.empty xs1
                      where
                        update ds v = DS.union 0 v ds
                    verifyQuery k =
                        IS.member k s1 == fst (DS.inSameSet 0 k s2)
                pure (all verifyQuery [1..500])
        specify "example 1" $ example $ do
            pending
