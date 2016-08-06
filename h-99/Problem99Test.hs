module Problem99Test where

import Test.Hspec

import Problem99
import Control.Arrow
import Data.Maybe
import Data.List
import qualified Data.Array as Arr
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM

{-# ANN module "HLint: ignore Redundant do" #-}

-- TODO: more detailed check
{-
  TODO: things need to be addressed:
  - for simplicity we rely on the correctness of "crosswordFromFile"
  - pre-defined letters are in right places
  - all words are used and nothing more
  - we don't want to rely on the uniqueness of the puzzle
    so we just take some input files but no hard-coded result for comparison
-}

{-# ANN checkResult "HLint: ignore Redundant ==" #-}
checkResult :: Crossword -> Bool -> Maybe Rect -> Bool
checkResult (CW ws (FW dim sites hints)) expectJust actual = case actual of
    Nothing -> expectJust == False
    Just rect ->
        let areDimsMatch = ((1,1), dim) == Arr.bounds rect
            siteCoords (Site l c dir) = take l (iterate next c)
              where
                next = case dir of
                    DH -> second succ
                    DV -> first succ
            -- all cells that are supposed to contain letters
            letterCells = S.fromList (concatMap siteCoords sites)
            isMatchingShape = all checkCell (Arr.assocs rect)
              where
                checkCell (coord,content) =
                    isJust content == coord `S.member` letterCells
            areHintsMatching = all checkHint (M.toList hints)
              where
                checkHint (c,ch) = rect Arr.! c == Just ch
            -- extract the corresponding word given a site
            -- "fromJust" should be safe as the check should have been done
            -- with "isMatchingShape"
            extractWord = map (fromJust . (rect Arr.!)) . siteCoords
            allFilledWords = map extractWord sites
            correctWordOccurrence = sort (concat $ IM.elems ws) == sort allFilledWords
        in areDimsMatch -- 1. dimension should match
           && isMatchingShape -- 2. in desired shape: cells are properly filled.
           && areHintsMatching -- 3. hints should be respected
           && correctWordOccurrence -- 4. all words are used, no less, no more

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
