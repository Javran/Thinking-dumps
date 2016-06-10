module Ch05PairingHeapTest where

import Test.Hspec
import Test.QuickCheck
import Data.List (sort)

import Ch05PairingHeap

{-# ANN module "HLint: ignore Redundant do" #-}

main :: IO ()
main = hspec $ do
    describe "PairingHeap" $ do
      it "can sort ordered elements" $
        property $ \xs -> sort (xs :: [Int]) == (toAscList . fromList) xs
