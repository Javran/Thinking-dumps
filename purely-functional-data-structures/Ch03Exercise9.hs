{-# LANGUAGE ScopedTypeVariables #-}
module Ch03Exercise9 where

import Test.Hspec
import Test.QuickCheck
import Ch03RedBlack
import Control.Monad.State


{-# ANN module "HLint: ignore Redundant do" #-}


{-

I think there are two ways of doing this (could be more)

* first notice that a tree of any size n can be built in the same "shape"
  regardless of what the actual value is. this suggests if we can figure out
  the shape of the tree given just the number n, we can build it from a sorted list
  by just putting the right values at the right nodes.

  However I'm not sure if recursion works in this case: the obvious idea
  is to say, given number n, we can pinpoint the root node, leaving n-1 values
  to be arranged into 2 subtrees. The we can solve the "small" problem by thinking about
  these 2 subtrees. But the problem is, I didn't find an obvious way to enforce the depth
  of 2 subtrees to be the same. I feel some clever way of constructing these 2 subtrees
  is needed for this idea to work.

* another idea, comes from http://cs.stackexchange.com/a/26300 (thanks to @FrankW).
  and I think this is easier to implement.

  notice that:
  - a perfect binary tree is always a valid red-black tree with all nodes colored black.
  - a complete binary tree will always be a valid red-black tree
    if you color the last incompelete layer of nodes red and every other nodes black.

  knowing these facts, we can build a red-black tree from sorted list
  by making a complete binary tree,
  and coloring last incomplete layer red and every other nodes black.

-}

-- split a number n into (d, extra) such that n = (2^d-1) + extra
-- with maximum possible d
splitDepthExtra :: Int -> (Int, Int)
splitDepthExtra n = (d, n - fullSize)
  where
    fullSize = 2 ^ d - 1
    d = floor (logBase 2 (fromIntegral n + 1 :: Double))

buildTree :: forall a. (Int, Int) -> State [a] (Tree a)
buildTree (dep,extra)
    | dep == 0 = pure E
    | dep == 1 = case extra of
        0 -> consumeOne >>= \x -> pure (T Black E x E)
        1 -> do
            l <- consumeOne
            v <- consumeOne
            pure (T Black (T Red E l E) v E)
        2 -> do
            l <- consumeOne
            v <- consumeOne
            r <- consumeOne
            pure (T Black (T Red E l E) v (T Red E r E))
        _ -> error "wrong split"
    | dep > 1 = do
        let fullSubCount = 2 ^ (dep-1) :: Int
            (lSize, rSize) = if extra <= fullSubCount
                               then (extra,0)
                               else (fullSubCount, extra-fullSubCount)
        lTree <- buildTree (dep-1,lSize)
        v <- consumeOne
        rTree <- buildTree (dep-1,rSize)
        pure (T Black lTree v rTree)
    | otherwise = error "wrong depth"
  where
    consumeOne :: State [a] a
    consumeOne = gets head <* modify tail

fromOrdList :: [a] -> Tree a
fromOrdList xs = evalState (buildTree (splitDepthExtra l)) xs
  where
    l = length xs

main :: IO ()
main = hspec $ do
    describe "fromOrdList" $ do
      it "should create valid red-black trees" $ do
        property $ do
            -- randomly picking up a list of elements
            n <- choose (0,1000 :: Int)
            let t = fromOrdList [1..n]
            pure $ checkRedBlackTree t
