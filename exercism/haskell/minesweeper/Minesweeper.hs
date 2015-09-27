module Minesweeper
  ( annotate
  ) where

import qualified Data.Set as S

type MineField = S.Set (Int,Int)

annotate :: [String] -> [String]
annotate xs = [] -- TODO
  where
    mf = toMineField xs
    countMine (x,y) = length (filter (`S.member` mf) neighborhoods)
      where
        neighborhoods =
            [ (x-1,y-1), (x-1,y), (x-1,y+1)
            , (x  ,y-1),          (x  ,y+1)
            , (x+1,y-1), (x+1,y), (x+1,y+1)
            ]

toMineField :: [String] -> MineField
toMineField xs = S.fromList ys
  where
    rows = length xs
    cols = length (head xs)
    coords = [(x,y) | x <- [1..rows], y <- [1..cols]]
    ys = map fst
       $ filter ((== '*') . snd)
       $ zip coords (concat xs)
