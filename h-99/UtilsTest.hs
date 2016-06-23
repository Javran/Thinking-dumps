module UtilsTest where

import Test.Hspec

import Utils

{-# ANN module "HLint: ignore Redundant do" #-}

main :: IO ()
main = hspec $ do
    describe "pick" $ do
        specify "example 1" $ example $
          pick "abcd" `shouldBe`
          [ ('a', "bcd")
          , ('b', "acd")
          , ('c', "abd")
          , ('d', "abc")
          ]

