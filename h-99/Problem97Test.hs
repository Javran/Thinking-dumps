module Problem97Test where

import Test.Hspec
import Test.QuickCheck
import Data.Maybe
import qualified Data.Array as Arr
import Control.Monad

import Problem97

{-# ANN module "HLint: ignore Redundant do" #-}

-- you can get "sudoku17" from: http://staffhome.ecm.uwa.edu.au/~00013890/sudoku17
-- see detail: http://www.csse.uwa.edu.au/~gordon/sudokumin.php
loadSudoku17 :: IO (Arr.Array Int RawIntArray)
loadSudoku17 = mkArr . lines <$> readFile "sudoku17"
  where
    mkArr xs = Arr.listArray (1,l) xs
      where
        l = length xs

mainSpeedTest :: IO ()
mainSpeedTest = do
    let puzzleFile = "sudoku17"
    rawPuzzles <- lines <$> readFile puzzleFile
    let -- l = length rawPuzzles
        solveAndPrint (solvedCount, allCount) curPuzzleRaw = do
            let newSolvedCount = case solvePuzzle (mkPuzzle curPuzzleRaw) of
                    Just _ -> solvedCount + 1
                    Nothing -> solvedCount
            -- printf "%d/%d of %d\n" newSolvedCount (allCount+1) l
            pure (newSolvedCount, allCount+1) :: IO (Int,Int)
    result <- foldM solveAndPrint (0,0) rawPuzzles
    putStrLn $ "all done: " ++ show result

mainHspec :: IO ()
mainHspec = do
    puzzles <- loadSudoku17
    let puzzleBounds = Arr.bounds puzzles
    hspec $ do
        describe "solvePuzzle" $ do
            specify "random 17-hint puzzles" $ do
                property $ do
                    puzzleInd <- choose puzzleBounds
                    pure (isJust . solvePuzzle . mkPuzzle $ puzzles Arr.! puzzleInd)

main :: IO ()
main = mainHspec
-- main = mainSpeedTest
