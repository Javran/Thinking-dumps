{-# LANGUAGE ScopedTypeVariables #-}
module Problem84Test where

import Test.Hspec

import Problem84

{-# ANN module "HLint: ignore Redundant do" #-}

main :: IO ()
main = hspec $ do
    describe "prim" $ do
        specify "example 1" $ example $
          let result =
                prim
                  [1 :: Int,2,3,4,5]
                  [(1,2,12),(1,3,34)
                  ,(1,5,78),(2,4,55)
                  ,(2,5,32),(3,4,61)
                  ,(3,5,44),(4,5,93)]
              getSum = sum . map (\(_,_,v) -> v)
              actualSum = getSum result
              expectedSum = getSum
                              [(1,2,12) :: (Int,Int,Int)
                              ,(1,3,34),(2,4,55),(2,5,32)]
          in expectedSum `shouldBe` actualSum
