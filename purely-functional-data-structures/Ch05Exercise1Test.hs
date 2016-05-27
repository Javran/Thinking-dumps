module Ch05Exercise1Test where

import Test.Hspec
import Test.QuickCheck
import Data.Foldable
import Data.List
import qualified Ch05Exercise1 as D

{-# ANN module "HLint: ignore Redundant do" #-}

-- using the same code to test set behavior
-- but this time we just want to test insertion
main :: IO ()
main = hspec $ do
    describe "Deque" $ do
      it "inserts correctly" $ do
        property $ do
            -- we make a list of numbers of both non-negatives and negatives
            -- if the number is non-negative, we insert it in front of the deque
            -- otherwise we insert it to the back of the current deque
            -- so we can end up with operations that randomly insert either in front of
            -- or to the back of the current deque, and the result is predictable
            xs <- listOf (choose (-500,500 :: Int))
            let ins dq i =
                    if i >= 0
                       then D.cons i dq
                       else D.snoc dq i
                resultActual = foldl' ins D.empty xs
                (as,bs) = partition (>= 0) xs
                resultExpect = reverse as ++ bs
            pure $ D.toList resultActual == resultExpect
