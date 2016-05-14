module Ch03Exercise9 where

import Ch03RedBlack

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
    fullSize = d ^(2 :: Int) - 1
    d = floor (logBase 2 (fromIntegral n + 1 :: Double))
