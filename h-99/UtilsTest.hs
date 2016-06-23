module UtilsTest where

import Test.Hspec
import Test.QuickCheck

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
        specify "can recover original list" $
          property $ \xs ->
            let -- ys: [(a,bcd),(b,acd) ...]
                ys = pick (xs :: [Int])
                -- ys1: [(0,(a,bcd)), (1,(b,acd)) ...]
                ys1 = zip [0..] ys
                recover (i,(c,cs)) = l ++ c:r
                  where
                    (l,r) = splitAt i cs
            in all (\rs -> recover rs == xs) ys1
