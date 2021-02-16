module Squares
  ( sumOfSquares
  , squareOfSum
  , difference
  )
where

square :: Integral a => a -> a
square x = x * x

sumOfSquares :: Integral a => a -> a
sumOfSquares = sum . map square . enumFromTo 1

squareOfSum :: Integral a => a -> a
squareOfSum = square . sum . enumFromTo 1

difference :: Integral a => a -> a
difference x = abs (sumOfSquares x - squareOfSum x)
