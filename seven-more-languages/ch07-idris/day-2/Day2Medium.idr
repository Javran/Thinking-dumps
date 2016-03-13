module Day2Medium

import Data.Vect
import Day2Easy

-- write a data type to hold the pixels in a display,
-- taking into account color and size

-- each Integer i should satisfy 0 <= i && i <= 255
-- but I won't enforce that for simplicity
data Color = RGB Integer Integer Integer

Pixels : (m : Nat) -> (n : Nat) -> Type
Pixels m n = Matrix m n Color
