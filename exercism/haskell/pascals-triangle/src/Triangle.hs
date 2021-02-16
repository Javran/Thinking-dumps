module Triangle
  ( rows
  , triangle
  )
where

triangle :: [[Integer]]
triangle = iterate next [1]
  where
    next xs = zipWith (+) (0 : xs) (xs ++ [0])

rows :: Int -> [[Integer]]
rows n = take n triangle
