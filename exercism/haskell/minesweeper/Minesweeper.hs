module Minesweeper
  ( annotate
  ) where

import qualified Data.Set as S
import Data.List.Split

type MineField = S.Set (Int,Int)

annotate :: [String] -> [String]
annotate xs = numOrMine
  where
    cols = length (head xs)
    -- step 1.
    -- [ "*  *"
    -- , " ** "
    -- , "**  "
    -- ]
    -- =>
    -- [ ((1,1),'*'), ((1,2),' ') ... ]
    ys = flattenWithCoord xs
    -- step 2.
    -- extract "(<coord>, '*')" to form a set of minefield coords
    mineField :: MineField
    mineField = S.fromList
              . map fst
              . filter ((== '*') . snd)
              $ ys
    -- for counting how many mines are around
    countMine (x,y) = length (filter (`S.member` mineField) neighborhoods)
      where
        neighborhoods =
            [ (x-1,y-1), (x-1,y), (x-1,y+1)
            , (x  ,y-1),          (x  ,y+1)
            , (x+1,y-1), (x+1,y), (x+1,y+1)
            ]
    -- step 3. from the flatten representation,
    -- count mine if there isn't a mine
    -- and convert back to the original representation
    numOrMine = map concat
              . chunksOf cols
              $ map idOrCount ys
      where
        idOrCount (_,'*') = "*"
        idOrCount (coord,_) = case cnt of
            0 -> " "
            _ -> show cnt
          where
            cnt = countMine coord

flattenWithCoord :: [String] -> [((Int,Int),Char)]
flattenWithCoord xs = zip coords (concat xs)
  where
    -- assumption on input:
    -- 1. non-empty
    -- 2. all rows are of same length
    rows = length xs
    cols = length (head xs)
    coords = [(x,y) | x <- [1..rows], y <- [1..cols]]
