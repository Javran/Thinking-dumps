{-# LANGUAGE ScopedTypeVariables #-}
module Problem87Test where

import Test.Hspec

import Problem87

{-# ANN module "HLint: ignore Redundant do" #-}

main :: IO ()
main = hspec $ do
    describe "depthFirst" $ do
        let g1 = ([1 :: Int,2,3,4,5,6,7], [(1,2),(2,3),(1,4),(3,4),(5,2),(5,4),(6,7)])
        specify "example 1" $ do
            depthFirst g1 1 `shouldBe` [1..5]
            depthFirst g1 7 `shouldBe` [7,6]
