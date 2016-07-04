{-# LANGUAGE ScopedTypeVariables #-}
module Problem86Test where

import Test.Hspec
import qualified Data.Map.Strict as M
import Data.Maybe
import Control.Monad

import Problem86

{-# ANN module "HLint: ignore Redundant do" #-}

checkColors :: (Ord a, Eq color) => [(a,a)] -> [(a,color)] -> Bool
checkColors es colors = checkLen && isJust (mapM_ checkEdge es)
  where
    colorMap = M.fromList colors
    -- each node should be color only once,
    -- we verify this by checking length of input and output structure
    checkLen = M.size colorMap == length colors
    checkEdge (a,b) = do
        ca <- M.lookup a colorMap
        cb <- M.lookup b colorMap
        guard $ ca /= cb

main :: IO ()
main = hspec $ do
    describe "kcolor" $ do
        specify "example 1" $ do
            let vs = ['a','b','c','d','e','f','g','h','i','j']
                es = [('a','b'),('a','e'),('a','f'),('b','c')
                     ,('b','g'),('c','d'),('c','h'),('d','e')
                     ,('d','i'),('e','j'),('f','h'),('f','i')
                     ,('g','i'),('g','j'),('h','j')]
            kcolor vs es `shouldSatisfy` checkColors es
