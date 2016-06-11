module Problem90Test where

import Test.Hspec
import Test.QuickCheck
import qualified Data.List as L
import Problem90

{-# ANN module "HLint: ignore Redundant do" #-}

main :: IO ()
main = hspec $ do
    describe "queens" $ do
      it "gives correct answers" $
        let test n = all verify (queens n)
              where
                verify solution = L.sort solution == [1..n]
        in property $ all test [1..10]
