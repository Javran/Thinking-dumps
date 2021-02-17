module Triplet
  ( tripletsWithSum
  )
where

import Control.Monad

tripletsWithSum :: Int -> [(Int, Int, Int)]
tripletsWithSum s = do
  c <- takeWhile (>= 5) $ iterate pred (s `quot` 2)
  let cSq = c * c
  b <- [c -1, c -2 .. (c -1) `quot` 2 + 1]
  let a = s - b - c
  guard $ a < b && a * a + b * b == cSq
  pure (a, b, c)
