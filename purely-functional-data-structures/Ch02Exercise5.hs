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

-- but this time we just want to test insertion
main :: IO ()
main = hspec $ do
    describe "complete v d" $ do
      it "should produce 2^d-1 elements" $ do
          forM_ [0 :: Int ..10] $ \d ->
              toAscList (complete 1 d) `shouldBe` replicate (2^d-1) 1
