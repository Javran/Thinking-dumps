module Ch03Exercise5 where

import Test.Hspec
import Test.QuickCheck
import qualified Data.List as L
import Data.Foldable

import Ch03Binomial hiding (findMin)

{-# ANN module "HLint: ignore Redundant do" #-}
-- exercise: define "findMin" directly rather than via a call to "viewMinTree"

findMin :: Ord a => Heap a -> Maybe a
findMin ts = fst <$> viewMin ts

main :: IO ()
main = hspec $ do
    describe "Binomial" $ do
      it "find the minimal element correctly" $ do
        let getMin :: Ord a => [a] -> Maybe a
            getMin [] = Nothing
            getMin xs = Just $ minimum xs
        property $ \xs -> (findMin . fromList) (xs :: [Int]) == getMin xs
