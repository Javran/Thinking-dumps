{-# LANGUAGE ScopedTypeVariables #-}
module Ch02Exercise5 where
import Test.Hspec
import Ch02BST
import Control.Monad
import Data.Maybe

{-# ANN module "HLint: ignore Redundant do" #-}

-- I think "complete binary tree" refers to BSTs
-- that is of depth d and all possible nodes are occupied.
-- in wikipedia this is actually defined as "perfect binary tree"

-- to show this runs in O(d) time, we just need to notice following facts:
-- + case analysis is done on the natural number
-- + at most one recursive call in any branch of the function
complete :: a -> Int -> BST a
complete _ 0 = E
complete v s = T sub v sub
  where
    sub = complete v (s-1)
{-
**to show "balanced _ n" runs in O(log n) time:**

- base case when n == 0,1 it runs in constant time
- induction step: assume n == k runs in O(log k) time

let's also assume "create2 k" runs in O(log k) time,

then "balanced (k+k)" makes recursive call to either "balanced k"
or "create2 k", and both of then runs in O(log k) time.

assume extra cost is a constant that has constant upbound C,

we know "balanced (k+k)" runs in O(log k) + O(log k) + C = O(log k) time.

so to summarize, "balanced n" runs in O(log n) time.

**now we need to show "create2 n" runs in O(log n) time**

- "create2 n" creates 2 trees by creating a balanced one
  then add or remove one from it.
- tree creation is done by recursive call to "balanced (n/2)", which takes O(log n) time
- then it takes O(d) time to add one node to or remove one from the resulting tree
  where d is bounded by O(log n), since we know the resulting tree is balanced,
  we know d is bounded by O(log n)
- therefore the time complexity of "create2 n" is O(log n) + O(log n) = O(log n)

-}
balanced :: forall a. a -> Int -> BST a
balanced _ 0 = E
balanced v n
    | odd n =
        -- when nodes (excluding root node)
        -- can be divided perfectly into 2 parts:
        let sub = balanced v halfN
        in T sub v sub
    | otherwise =
        -- when sub nodes can be perfectly divided:
        -- we'll put one extra node on right subtree
        -- by using create2
        let (l,r) = create2 halfN
        in T l v r
  where
    halfN = (n-1) `div` 2

    -- "(l,r) = create2 m" produces 2 subtrees:
    -- l has m elements
    -- r has m+1 elements
    create2 :: Int -> (BST a, BST a)
    -- our strategy is: make a balanced one,
    -- then add one node and remove one node from it accordingly
    create2 m
        | odd m =
            -- we can make a balanced tree with m nodes
            let subL = balanced v m
            -- add one to produce the balanced tree that has m+1 nodes
            in (subL, oneMore m subL)
        | otherwise =
            -- we can make a balanced tree with m+1 nodes
            let mp1 = m+1
                subR = balanced v mp1
            -- remove one to produce the balanced tree that has m nodes
            in (oneLess mp1 subR, subR)

    -- | INVARIANT for "oneMore n t" and "oneLess n t"
    -- * size t == n
    -- * (oneLess only) n /= 0
    -- * t is always balanced
    -- * one of the following must be true for all nodes t1 in t:
    --     * size (left t1)    == size (right t1)
    --     * size (left t1) +1 == size (right t1)
    oneMore :: Int -> BST a -> BST a
    oneLess :: Int -> BST a -> BST a

    -- base cases
    oneMore 0 E = T E v E
    oneMore 1 t'@(T E _ E) = T E v t'
    oneMore m (T l _ r)
        | even (m-1) =
            -- left subtree and right subtree has same number of nodes
            -- we will then add one to right part to maintain the invariant
            T l v (oneMore halfM r)
        | otherwise =
            -- right subtree has one more element than left one does
            -- so we add one to left part to balance the tree
            T (oneMore halfM l) v r
      where
        halfM = (m-1) `div` 2
    oneMore _ _ = error "oneMore: invariant violated"

    oneLess 1 (T E _ E) = E
    oneLess m (T l _ r)
        | even (m-1) =
            -- left subtree and right subtree has same number of nodes
            -- we will then remove one from left part to maintain the invariant
            T (oneLess ((m-1) `div` 2) l) v r
        | otherwise =
            -- right subtree has one more element than left one does
            -- so we remove one from right part to maintain the invariant
            T l v (oneLess ((m-1) `div` 2 + 1) r)
    oneLess _ _ = error "oneLess: invariant violated"

isBalanced :: BST a -> Bool
isBalanced = isJust . isBalancedM
  where
    -- test whether a tree is balanced,
    -- return number of nodes upon success
    isBalancedM :: BST a -> Maybe Int
    isBalancedM E = Just 0
    isBalancedM (T l _ r) = do
        nL <- isBalancedM l
        nR <- isBalancedM r
        guard $ abs (nL - nR) <= 1
        pure (nL+nR+1)

main :: IO ()
main = hspec $ do
    describe "complete v d" $ do
      it "should produce 2^d-1 elements" $ do
          forM_ [0 :: Int ..10] $ \d ->
              toAscList (complete 1 d) `shouldBe` replicate (2^d-1) (1 :: Int)
    describe "balanced v n" $ do
      it "should produce n elements" $ do
          forM_ [0 :: Int ..127] $ \n -> do
              let tree = balanced 1 n
              toAscList tree `shouldBe` replicate n (1 :: Int)
              isBalanced tree `shouldBe` True
