module Transpose
  ( transpose
  )
where

import qualified Data.List as L

transpose :: [String] -> [String]
transpose = L.transpose . padder

{-
  adds padding by maintaining a running maximum length from bottom.

  example input:

  > 4444
  > 7777777
  > 333
  > 666666
  > 22
  > 55555
  > 1

  output (`_` are padding spaces):

  > 4444___
  > 7777777
  > 333___
  > 666666
  > 22___
  > 55555
  > 1

 -}
padder :: [String] -> [String]
padder [] = []
padder xs = reverse $ y : L.unfoldr go (ys, length y)
  where
    y : ys = reverse xs
    go ([], _) = Nothing
    go (z : zs, maxLen) = do
      let zLen = length z
      Just $
        if zLen > maxLen
          then (z, (zs, zLen))
          else (z <> replicate (maxLen - zLen) ' ', (zs, maxLen))
