{-# LANGUAGE ScopedTypeVariables #-}
module Problem88Test where

import Test.Hspec
import Data.Function
import Data.List

import Problem88

{-# ANN module "HLint: ignore Redundant do" #-}

main :: IO ()
main = hspec $ do
    describe "connectedComponents" $ do
        let g1 = ([1 :: Int ..7], [(1,2),(2,3),(1,4),(3,4),(5,2),(5,4),(6,7)])
        specify "example 1" $ do
            connectedComponents g1 `shouldSatisfy` ccEq [[1,2,3,4,5],[6,7]]
  where
    -- equivalence between group of connected components
    -- note that "Ord" instance is not a requirement, but having
    -- this instance around will make testcase writing a lot easier.
    -- (despite being a bit less efficient)
    ccEq :: Ord a => [[a]] -> [[a]] -> Bool
    ccEq = (==) `on` (sort . map sort)
