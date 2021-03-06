module Ch02Exercise3 where

import Test.Hspec
import Test.QuickCheck
import qualified Data.IntSet as IS
import Ch02BST hiding (insert)

import Data.Maybe

{-# ANN module "HLint: ignore Redundant do" #-}

-- the proper way of implementing "exception" is to make use of Maybe
-- let's try doing it.
-- instead of duplicating nodes before the recursive call,
-- we do insertion from leaf to root, if we can find
-- the value in the tree, then we immediately know
-- there is no need duplicating any node of the tree.
insertM :: Ord a => a -> BST a -> Maybe (BST a)
insertM v E = Just $ T E v E
insertM v (T l x r)
    | v == x = Nothing
    | v <  x = (\newL -> T newL x r) <$> insertM v l
    | v >  x = T l x <$> insertM v r
    | otherwise = error "impossible"

insert :: Ord a => a -> BST a -> BST a
insert v t = fromMaybe t (insertM v t)

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
