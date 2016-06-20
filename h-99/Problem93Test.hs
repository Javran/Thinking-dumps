module Problem93Test where

import Test.Hspec
import Test.QuickCheck

import Problem93

{-# ANN module "HLint: ignore Redundant do" #-}

main :: IO ()
main = hspec $ do
    describe "splits" $ do
        specify "example 1" $ example $
          splits [1 :: Int,3,2] `shouldBe`
            [([],[1,3,2])
            ,([1],[3,2])
            ,([1,3],[2])
            ,([1,3,2],[])]
        specify "can recover original list" $
            property $ \xs ->
              let result = splits (xs :: [Int])
              in all (\(as,bs) -> xs == as ++ bs) result
