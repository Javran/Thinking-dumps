module Day2Medium

import Data.Vect
import Data.So
import Day2Easy

-- write a data type to hold the pixels in a display,
-- taking into account color and size

-- each Integer i should satisfy 0 <= i && i <= 255
-- but I won't enforce that for simplicity
data Color = RGB Integer Integer Integer

Pixels : (m : Nat) -> (n : Nat) -> Type
Pixels m n = Matrix m n Color

-- transpose a matrix
matTranspose : Matrix m n a -> Matrix n m a
  -- pattern matching to destruct the first row,
  -- we know all rows are of same size, and we can do things recursively
matTranspose ((x :: xs) :: ys) = (x :: map head ys) :: matTranspose (xs :: map tail ys)
  -- corner case: when all elements are consumed, we append "[]" to finish the list
matTranspose ([] :: xs) = []
  -- corner case: when the list is empty, we need to return something.
  -- looks like the opposite of previous corner case:
  -- when we get [], we need n "[]"s in return
  -- so we need to pattern matching on hidden parameter {n}.
matTranspose {n} [] = replicate n []

data BndInt : (l : Integer) -> (h : Integer) -> (x : Integer) -> Type where
  BI : (l : Integer) -> (h : Integer) -> (x : Integer) -> So (l <= x && x <= h) -> BndInt l h x

boundedInt : (l : Integer) -> (h : Integer) -> (x : Integer) -> Maybe (BndInt l h x)
boundedInt l h x = case choose (l <= x && x <= h) of
  Left valid => Just (BI l h x valid)
  Right _ => Nothing
