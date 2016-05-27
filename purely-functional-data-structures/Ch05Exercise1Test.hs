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
            xs <- listOf (choose (-500,500 :: Int))
            let ins dq i =
                    if i >= 0
                       then D.cons i dq
                       else D.snoc dq i
                resultActual = foldl' ins D.empty xs
                (as,bs) = partition (>= 0) xs
                resultExpect = reverse as ++ bs
            pure $ D.toList resultActual == resultExpect
