module Minesweeper
  ( annotate
  ) where

import qualified Data.Set as S

type MineField = S.Set (Int,Int)

annotate :: [String] -> [String]
annotate = undefined

toMineField :: [String] -> MineField
toMineField xs = S.fromList ys
  where
    rows = length xs
    cols = length (head xs)
    coords = [(x,y) | x <- [1..rows], y <- [1..cols]]
    ys = map fst
       $ filter ((== '*') . snd)
       $ zip coords (concat xs)
