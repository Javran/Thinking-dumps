module Problem90Test where

import Test.Hspec
import Test.QuickCheck
import qualified Data.List as L
import qualified Data.IntSet as IS
import Problem90

{-# ANN module "HLint: ignore Redundant do" #-}

main :: IO ()
main = hspec $ do
    describe "queens" $ do
      it "gives correct answers" $
        let test n = all verify (queens n)
              where
                verify solution = check1 && check2 && check3
                  where
                    allUnique :: [Int] -> Bool
                    allUnique xs = length xs == IS.size (IS.fromList xs)
                    fullSolution = zip [1..n] solution
                    check1 = L.sort solution == [1..n]
                    check2 = allUnique (map (uncurry (+)) fullSolution)
                    check3 = allUnique (map (uncurry subtract) fullSolution)
        in property $ all test [1..10]
