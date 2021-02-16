{-# LANGUAGE MonadComprehensions #-}

module Grains
  ( square
  , total
  )
where

-- square 1 = 1
-- square n = 2 * square (n-1)
square :: Integer -> Maybe Integer
square x = [2 ^ (x -1) | x > 0, x <= 64]

-- total = sum $ map (2^) [1..64]
--       = 2^0 + (2^1 + ... + 2^63) - 2^0
--       = 2^64 - 1
total :: Integer
total = 2 ^ (64 :: Int) - 1
