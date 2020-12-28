module Triangle
  ( TriangleType(..)
  , triangleType
  )
where

data TriangleType
  = Equilateral
  | Isosceles
  | Scalene
  | Illogical
    deriving (Eq, Show)

-- invariant: (a,b,c) = triSort _ implies a <= b <= c
--   and the output is a permutation of input
triSort :: (Int,Int,Int) -> (Int,Int,Int)
triSort (a,b,c)
  | a <= b && a <= c = if b <= c then (a,b,c) else (a,c,b)
  | b <= a && b <= c = if a <= c then (b,a,c) else (b,c,a)
  | c <= a && c <= b = if a <= b then (c,a,b) else (c,b,a)
  | otherwise = error "impossible"

triangleType :: Int -> Int -> Int -> TriangleType
triangleType a b c =
    -- a triangle must meet the following condition
    -- * y < z + x (implied)
    -- * z < x + y (to be verified)
    -- * x < z + y (implied)
    if z < x + y && x > 0
      then if x == z
              -- x == y == z (implied)
              then Equilateral
              else if x == y || y == z
                     -- by definition
                     then Isosceles
                     else Scalene
      else Illogical
  where
    -- invariant: x <= y <= z, assume x > 0
    -- therefore:
    -- * x <= z
    -- * x < z + y
    -- * y <= z
    -- * y < z + x
    (x,y,z) = triSort (a,b,c)

