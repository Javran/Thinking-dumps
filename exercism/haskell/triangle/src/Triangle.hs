{-# LANGUAGE MultiWayIf #-}

module Triangle
  ( TriangleType (..)
  , triangleType
  )
where

data TriangleType
  = Equilateral
  | Isosceles
  | Scalene
  | Illegal
  deriving (Eq, Show)

{-
  INVARIANT: (a,b,c) = triSort _ implies a <= b <= c
  and the output is a permutation of input
 -}
triSort :: Ord a => (a, a, a) -> (a, a, a)
triSort (a, b, c) = (a2, b3, c1)
  where
    minMax x y = if x <= y then (x, y) else (y, x)
    {-
      Sorting network:
      a ----> a1 ----= a1 ----> a2
          x                 x
      b ----> b1 ----> b2 ----> b3
                   x
      c ----= c  ----> c1 ----= c1
     -}
    (a1, b1) = minMax a b
    (b2, c1) = minMax b1 c
    (a2, b3) = minMax a1 b2

triangleType :: (Num a, Ord a) => a -> a -> a -> TriangleType
triangleType a b c =
  {-
    a triangle must meet the following condition:
    - y < z + x (implied)
    - z < x + y (to be verified)
    - x < z + y (implied)
   -}
  if
      | x <= 0 || z >= x + y -> Illegal
      | x == z -> Equilateral
      | x == y || y == z -> Isosceles
      | otherwise -> Scalene
  where
    (x, y, z) = triSort (a, b, c)
