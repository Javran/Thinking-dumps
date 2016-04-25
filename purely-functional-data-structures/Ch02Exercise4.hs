module Ch02Exercise4 where

import Test.Hspec
import Test.QuickCheck
import qualified Data.IntSet as IS
import Ch02BST hiding (member, insert)
import Ch02Exercise2 (member)
import Ch02Exercise3 (insert)

{-# ANN module "HLint: ignore Redundant do" #-}

-- nothing special, I think just copying code from Ex 2.2 and Ex 2.3 will do
-- TODO: do performance measurement?

fromList :: Ord a => [a] -> BST a
fromList = makeFromList insert

-- using the same code to test set behavior
-- but this time we just want to test insertion
main :: IO ()
main = hspec $ do
    describe "member" $ do
      it "should be the same as Data.IntSet" $ do
        property $ do
            -- randomly picking up a list of elements
            xs <- listOf (choose (0,1000 :: Int))
            let s1 = IS.fromList xs
                s2 = fromList xs
            -- then test membership on all values within this range
            -- and their results should be the same.
            pure $ all (\x -> member x s2 == IS.member x s1) [0..1000]
