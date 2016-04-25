module Ch02Exercise3 where

import Test.Hspec
import Test.QuickCheck
import qualified Data.IntSet as IS

import Data.Maybe

{-# ANN module "HLint: ignore Redundant do" #-}

data BST a = E | T (BST a) a (BST a) deriving (Show)

-- nothing special, I think just copying code from Ex 2.2 and Ex 2.3 will do
-- TODO: less duplicated code
-- TODO: do performance measurement?

-- copied from ex 2.2
member :: Ord a => a -> BST a -> Bool
member _ E = False
member v tree@(T _ curX _) = go tree curX
  where
    -- keep is the value that probably equals to v
    go E keep = v == keep
    go (T l x r) keep = if v <= x
        then
          -- we know v <= x, in which x should be "closer"
          -- to v than keep
          go l x
        else
          -- we know v > x, it's clear that x
          -- cannot be equal to v, so we keep "keep" value
          go r keep

-- copied from ex 2.3
insert :: Ord a => a -> BST a -> BST a
insert v t = fromMaybe t (insertM v t)
  where
    insertM :: Ord a => a -> BST a -> Maybe (BST a)
    insertM v E = Just $ T E v E
    insertM v (T l x r)
        | v == x = Nothing
        | v <  x = (\newL -> T newL x r) <$> insertM v l
        | v >  x = T l x <$> insertM v r
        | otherwise = error "impossible"

fromList :: Ord a => [a] -> BST a
fromList = foldr insert E

toAscList :: BST a -> [a]
toAscList E = []
toAscList (T l v r) = toAscList l ++ v : toAscList r

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
