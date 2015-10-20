module Triplet
  ( mkTriplet
  , isPythagorean
  , pythagoreanTriplets
  ) where

type Triplet a = (a, a, a)

mkTriplet :: a -> a -> a -> Triplet a
mkTriplet = (,,)

isPythagorean :: Integral a => Triplet a -> Bool
isPythagorean (x,y,z) = x*x + y*y == z*z

pythagoreanTriplets :: Integral a => a -> a -> [Triplet a]
pythagoreanTriplets from to = _
