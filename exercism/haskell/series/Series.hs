module Series
  ( slices
  ) where

import Data.Char
import Data.List

slices :: Int -> String -> [[Int]]
slices sLen xs = take (l - sLen + 1) . map (take sLen) . tails $ ds
  where
    l = length xs
    ds = map (\x -> ord x - ord '0') xs
