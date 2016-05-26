module Ch05QueueTest where

import Test.Hspec
import Test.QuickCheck
import Data.Foldable

import Ch05Queue as Q

{-# ANN module "HLint: ignore Redundant do" #-}
main :: IO ()
main = hspec $ do
    describe "Queue" $ do
      it "inserts correctly" $ do
          property $ \ xs ->
              Q.toList (foldl' Q.snoc Q.empty xs) == (xs :: [Int])
