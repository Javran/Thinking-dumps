module Squares
  ( sumOfSquares
  , squareOfSums
  , difference
  ) where

square :: Integral a => a -> a
square x = x*x

sumOfSquares :: Integral a => a -> a
sumOfSquares = sum . map square . enumFromTo 1

squareOfSums :: Integral a => a -> a
squareOfSums = square . sum . enumFromTo 1

difference :: Integral a => a -> a
difference x = abs (sumOfSquares x - squareOfSums x)
