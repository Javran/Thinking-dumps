module DisjointSetTest where

import Test.Hspec
import Test.QuickCheck
import Data.Foldable
import Data.List
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
            -- create two small sets: [1,2,3,4] [5,6,7,8,9,10]
            let pairs = [(1,2),(2,3),(4,3),(5,6),(10,7),(8,10),(9,7),(6,7 :: Int)]
                compareGroups g1 g2 = normalize g1 == normalize g2
                    where
                      normalize = sort . map sort
                s1 = foldl' (\ds (a,b) -> DS.union a b ds) DS.empty pairs
                mkPair xs ys = [(a,b) | a <- xs, b <- ys]
                testSameSet xs ds = all (\(a,b) -> fst $ DS.inSameSet a b ds) (mkPair xs xs)
                testDiffSet xs ys ds = all (\(a,b) -> not $ fst $ DS.inSameSet a b ds) (mkPair xs ys)
            -- test all possible queries
            s1 `shouldSatisfy` testSameSet [1..4]
            s1 `shouldSatisfy` testSameSet [5..10]
            s1 `shouldSatisfy` testDiffSet [1..4] [5..10]
            DS.toGroups s1 `shouldSatisfy` compareGroups [[1..4],[5..10]]
            -- connecting any two value belonging to the 2 different sets
            let connectedSets =
                    [ DS.union a b s1
                    | (a,b) <- mkPair [1..4] [5..10] ]
                verifyDS ds = all (\(a,b) -> fst $ DS.inSameSet a b ds) (mkPair xs xs)
                  where
                    xs = [1..10]
            -- now that all should be connected
            connectedSets `shouldSatisfy` all verifyDS
            connectedSets `shouldSatisfy` all (compareGroups [[1..10]] . DS.toGroups)
