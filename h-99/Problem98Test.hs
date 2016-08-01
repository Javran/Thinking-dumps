module Problem98Test where

import Test.Hspec
import Data.List

import qualified Data.Array as Arr
import Problem98

{-# ANN module "HLint: ignore Redundant do" #-}

type RawPuzzle = [String]

puzzle1 :: RawPuzzle
puzzle1 =
    [ " XXX    "
    , "XX X    "
    , " XXX  XX"
    , "  XXXXXX"
    , "X XXXXX "
    , "    X   "
    , "   XX   "
    ]

-- intentionally leaving blank rules to see if it works properly
puzzle2 :: RawPuzzle
puzzle2 =
    [ "   XXXXX    "
    , " XXXXXXXXX  "
    , "XX X X X XX "
    , "XXXXXXXXXXX "
    , "     X      "
    , "     X      "
    , "     X      "
    , "     X      "
    , "     X X    "
    , "     XXX    "
    , "            "
    ]

puzzle3 :: RawPuzzle
puzzle3 =
    [ "XX XX X X   XX  XXXXX X  "
    , "X   X XXXX  XXX XXX X X X"
    , "X XXXX  XXXXXX   XXX  XX "
    , " XXXXXXXXX   X  X XXXXXXX"
    , " XXX X XXX  XXXXX  XXXXXX"
    , " XXXX  X XX XX XXX   XXXX"
    , " XX XX  XX XX  X XXXXXX  "
    , "  XX XXXXX       XXXXX  X"
    , "XXXX  XXX     XXX  XX  X "
    , "XXXX    X    XXXX  XX XXX"
    , "     XX    XXXX XXXXXX X "
    , "XXXXXXXXXX   XX X X  X XX"
    , "XXX X XX X   XXXXXXXXX X "
    , "XXXXXXXXXX   XXXXXXX XX X"
    , " X XXXXXXXXXX XX  XXXXXX "
    , "   XXX X XXXXXXXXXXXXX XX"
    , " XXXXXXXXXXXXXX XXX XXXXX"
    , " X  XX XXXXX XXXX X X XXX"
    , "X X X  XXXX X X X    XXX "
    , " XXXXXXXXX XX XX X XX XXX"
    , " X XX XXXXXXXXXX XX XXX  "
    , "X XX  X   X  XX XXXX     "
    , " XX  X XXXXX      XXX X X"
    , " XX XXXXXXXX  XXXXXX  X X"
    , " XXXXXXXX XXXX  X XXXXXX "
    ]

mkPuzzle :: RawPuzzle -> ([[Int]], [[Int]])
mkPuzzle raw = (rowRules, colRules)
  where
    raw1 = (map . map) cov raw
      where
        cov ' ' = False
        cov _ = True
    raw2 = transpose raw1

    rowRules = map lineToRule raw1
    colRules = map lineToRule raw2

    lineToRule = lineToRule' . dropWhile (/= True)

    lineToRule' [] = []
    lineToRule' xs
        | (trues,remained) <- span (== True) xs =
            length trues : lineToRule' (dropWhile (/= True) remained)

main :: IO ()
main = hspec $ do
    describe "solveRect" $ do
        let examplePuzzle rawPz = example $
              let ng@(NG nRows nCols _) = uncurry fromRawNonogram (mkPuzzle rawPz)
                  cov True = 'X'
                  cov False = ' '
                  arrToList =
                      take nRows
                    . unfoldr (Just . splitAt nCols)
                    . map cov
                    . Arr.elems
              in solveRect ng `shouldSatisfy` maybe False ((== rawPz) . arrToList)
        specify "puzzle 1" $ examplePuzzle puzzle1
        specify "puzzle 2" $ examplePuzzle puzzle2
        specify "puzzle 3" $ examplePuzzle puzzle3
