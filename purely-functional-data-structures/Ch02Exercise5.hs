{-# LANGUAGE ScopedTypeVariables #-}
module Ch02Exercise5 where
import Test.Hspec
import Test.QuickCheck
import qualified Data.IntSet as IS
import Ch02BST
import Control.Monad

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

balanced :: forall a. a -> Int -> BST a
balanced _ 0 = E
balanced v n
    | odd n =
        let sub = balanced v halfN
        in T sub v sub
    | otherwise =
        let (l,r) = create2 halfN
        in T l v r
  where
    halfN = (n-1) `div` 2

    create2 :: Int -> (BST a, BST a)
    create2 m
        | odd m =
            let subL = balanced v m
            in (subL, oneMore m subL)
        | otherwise =
            let mp1 = m+1
                subR = balanced v mp1
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

    oneMore 0 E = T E v E
    oneMore 1 t'@(T E _ E) = T E v t'
    oneMore m (T l _ r)
        | even (m-1) = T l v (oneMore ((m-1) `div` 2) r)
        | otherwise = T (oneMore ((m-1) `div` 2) l) v r
    oneMore _ _ = error "oneMore: invariant violated"

    oneLess 1 (T E _ E) = E
    oneLess m (T l _ r)
        | even (m-1) = T (oneLess ((m-1) `div` 2) l) v r
        | otherwise = T l v (oneLess ((m-1) `div` 2 + 1) r)
    oneLess _ _ = error "oneLess: invariant violated"

-- but this time we just want to test insertion
main :: IO ()
main = hspec $ do
    describe "complete v d" $ do
      it "should produce 2^d-1 elements" $ do
          forM_ [0 :: Int ..10] $ \d ->
              toAscList (complete 1 d) `shouldBe` replicate (2^d-1) 1
    describe "balanced v n" $ do
      it "should produce n elements" $ do
          forM_ [0 :: Int ..127] $ \n ->
              toAscList (balanced 1 n) `shouldBe` replicate n 1
