module Triangle
  ( row
  , triangle
  ) where

triangle :: Integral a => [ [a] ]
triangle = iterate next [1]
  where
    next xs = zipWith (+) (0:xs) (xs++[0])

row :: Integral a => Int -> [a]
row n = triangle !! (n-1)
