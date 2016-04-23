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

member :: Ord a => a -> BST a -> Bool
member v tree = case tree of
    E -> False
    T _ curX _ ->
        let go E y = v == y -- y is the value that probably equals to v
            go (T l x r) y = if v <= x
                                then go l x
                                else go r y
        in go tree curX

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
            xs <- listOf (choose (0,1000 :: Int))
            let s1 = IS.fromList xs
                s2 = fromList xs
            pure $ all (\x -> member x s2 == IS.member x s1) [0..1000]
