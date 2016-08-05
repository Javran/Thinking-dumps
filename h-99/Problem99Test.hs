module Problem99Test where

import Test.Hspec

import Problem99
import Data.Maybe

{-# ANN module "HLint: ignore Redundant do" #-}

-- TODO: more detailed check
checkResult :: Crossword -> Bool -> Maybe Rect -> Bool
checkResult cw expectJust actual = expectJust == isJust actual

main :: IO ()
main = hspec $ do
    describe "solvePuzzle" $ do
        let mkTestFromFile fp expectJust = do
                specify ("Testing file: " ++ fp) $ example $ do
                    cw <- crosswordFromFile fp
                    solvePuzzle cw
                        `shouldSatisfy` checkResult cw expectJust
        mkTestFromFile "p7_09a.dat" True
        mkTestFromFile "p7_09b.dat" True
        mkTestFromFile "p7_09c.dat" False
        mkTestFromFile "p7_09d.dat" True
