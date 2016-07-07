{-# LANGUAGE ScopedTypeVariables #-}
module Problem87Test where

import Test.Hspec

import Problem87

{-# ANN module "HLint: ignore Redundant do" #-}

{-
    9 -- 1 -- 7 -- 3
         |    |    |
         8 -- 2 -- 6 -- 4
                   |
                   5

-}

main :: IO ()
main = hspec $ do
    describe "depthFirst" $ do
        let g1 = ([1 :: Int ..7], [(1,2),(2,3),(1,4),(3,4),(5,2),(5,4),(6,7)])
            g2 = ([1 :: Int ..9], [(9,1),(1,7),(7,3),(1,8),
                                   (7,2),(3,6),(8,2),(2,6),(6,4),(6,5)])
        specify "example 1" $ do
            depthFirst g1 1 `shouldBe` [1..5]
            depthFirst g1 7 `shouldBe` [7,6]
        specify "example 2" $ do
            depthFirst g2 1 `shouldBe` [1,7,2,6,3,4,5,8,9]
            depthFirst g2 2 `shouldBe` [2,6,3,7,1,8,9,4,5]
