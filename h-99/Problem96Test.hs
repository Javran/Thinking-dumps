module Problem96Test where

import Test.Hspec

import Problem96

{-# ANN module "HLint: ignore Redundant do" #-}

main :: IO ()
main = hspec $ do
    describe "identifier" $ do
        specify "example 1" $ example $
            identifier "this-is-a-long-identifier" `shouldBe` True
        specify "example 2" $ example $
            identifier "this-ends-in-" `shouldBe` False
        specify "example 3" $ example $
            identifier "two--hyphens" `shouldBe` False
        specify "example 4" $ example $
            identifier "with-nums-1-2-3" `shouldBe` True
        specify "example 5" $ example $
            identifier "w1abc-123" `shouldBe` True
        specify "example 6" $ example $
            identifier "1abc-123" `shouldBe` False
