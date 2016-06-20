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
    describe "take2s" $ do
        specify "example 1" $ example $
          take2s [1 :: Int,2,3] `shouldBe`
            [((1,2),([],[3]))
            ,((2,3),([1],[]))
            ]
        specify "can recover original list" $
            property $ \xs ->
              let result = take2s (xs :: [Int])
              in all (\((a,b),(cs,ds)) -> xs == cs ++ a:b:ds) result
