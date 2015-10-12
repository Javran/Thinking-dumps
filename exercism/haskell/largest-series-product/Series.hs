module Series
  ( digits
  , slices
  , largestProduct
  ) where

import Data.Char
import Data.List

digits :: [] Char -> [] Int
digits = map (subtract (ord '0') . ord)

listSlices :: Int -> [a] -> [[a]]
listSlices slen xs =
      take nPieces
    . map (take slen)
    . tails
    $ cycle xs
  where
    len = length xs
    nPieces = len - slen + 1

slices :: Int -> String -> [[Int]]
slices n xs = listSlices n (digits xs)

largestProduct :: Int -> String -> Int
largestProduct n raw = case products of
    -- this doesn't make any sense,
    -- it's here simply because testcase wants it.
    [] -> 1
    _ -> maximum products
  where
    products = map product $ slices n raw
