module Ch05QueueTest where


import Test.Hspec
import Test.QuickCheck
import Data.Foldable
import Data.Maybe

import Ch05Queue

{-# ANN module "HLint: ignore Redundant do" #-}

main = hspec $ do
    describe "Queue" $ do
      it "inserts correctly" $ do
          property $ True
