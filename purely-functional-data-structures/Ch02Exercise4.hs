module Ch02Exercise4 where

import Test.Hspec
import Test.QuickCheck
import qualified Data.IntSet as IS
import Ch02BST hiding (insert)
import Data.Maybe

{-# ANN module "HLint: ignore Redundant do" #-}

insert :: Ord a => a -> BST a -> BST a
insert v tree = fromMaybe tree (insertM tree)
  where
    insertM E = Just (T E v E)
    insertM (T _ curX _) = go tree curX
      where
        -- keep is the value that probably equals to v
        go E keep = if v == keep then Nothing else Just (T E v E)
        go (T l x r) keep = if v <= x
            then
              -- we know v <= x, in which x should be "closer"
              -- to v than keep
              (\newL -> T newL x r) <$> go l x
            else
              -- we know v > x, it's clear that x
              -- cannot be equal to v, so we keep "keep" value
              T l x <$> go r keep

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
