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
    ys = flattenWithCoord xs
    mineField :: MineField
    mineField = S.fromList
              . map fst
              . filter ((== '*') . snd)
              $ ys
    countMine (x,y) = length (filter (`S.member` mineField) neighborhoods)
      where
        neighborhoods =
            [ (x-1,y-1), (x-1,y), (x-1,y+1)
            , (x  ,y-1),          (x  ,y+1)
            , (x+1,y-1), (x+1,y), (x+1,y+1)
            ]
    numOrMine = map concat
              . chunksOf cols
              $ map idOrCount ys
      where
        idOrCount (_,'*') = "*"
        idOrCount (coord,_) = show (countMine coord)

flattenWithCoord :: [String] -> [((Int,Int),Char)]
flattenWithCoord xs = zip coords (concat xs)
  where
    -- assumption on input:
    -- 1. non-empty
    -- 2. all rows are of same length
    rows = length xs
    cols = length (head xs)
    coords = [(x,y) | x <- [1..rows], y <- [1..cols]]
