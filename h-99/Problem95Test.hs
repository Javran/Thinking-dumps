module Problem95Test where

import Test.Hspec

import Problem95

{-# ANN module "HLint: ignore Redundant do" #-}

main :: IO ()
main = hspec $ do
    describe "fullWords" $ do
        specify "example 1" $ example $
            fullWords 0 `shouldBe` "zero"
        specify "example 2" $ example $
            fullWords 9 `shouldBe` "nine"
        specify "example 3" $ example $
            fullWords 175 `shouldBe` "one-seven-five"
        specify "example 4" $ example $
            fullWords 42836 `shouldBe` "four-two-eight-three-six"
