module Ch02Exercise2 where

import Test.Hspec
import Test.QuickCheck
import qualified Data.IntSet as IS

{-# ANN module "HLint: ignore Redundant do" #-}

{-

In book it says "in the worst case, `member` performs approximately 2d comparisons".

This is because in book, the implementation of `member` considers 3 branches:

if x < y then _branch1 else if x > y then _branch2 else _branch3

in the worst case we need to traverse to the deepest element of the tree,
by always taking _branch2, that results in 2d comparisons.

-}

data BST a = E | T (BST a) a (BST a) deriving (Show)

-- TODO: I'm not sure whether this is a correct implementation
-- and we probably need some prove.

{-

(See Arne Andersson "A note on searching in a binary search tree.")

Reasoning about the correctness of this implementation:

we consider 3 phases:

- when the value we want to find is not yet in "keep" variable

    since `v == x` is never met in this phase,
    so it goes exactly as binary search, except that only one comparison is done.
    if we reach a leaf during this phase, the element we want to find is clearly not
    in the tree we are searching

- by the time when we hit the node when `v == x` is met

    the value is then kept in `keep` by the recursive call `go l x`, which also goes to
    the left branch of it.

- by the time when we pass the node in which `v == x` is met.

    because in previous phase we have gone into the left tree,
    and the property of BST tells us all nodes in this subtree is less than `keep`
    (and also less than `v` because `v == keep`)
    so the implementation will always pick right subtree util a leaf node is hit.
    in this phase no update to "keep" will be performed.

-}
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

-- regular insert function
insert :: Ord a => a -> BST a -> BST a
insert x E = T E x E
insert x s@(T l y r)
    | x < y = T (insert x l) y r
    | x > y = T l y (insert x r)
    | otherwise = s

fromList :: Ord a => [a] -> BST a
fromList = foldr insert E

toAscList :: BST a -> [a]
toAscList E = []
toAscList (T l v r) = toAscList l ++ v : toAscList r

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
