module Triangle
    ( TriangleType(..)
    , triangleType
    )
where

import Data.List (sort)

data TriangleType
    = Equilateral
    | Isosceles
    | Scalene
    | Illogical
      deriving (Eq, Show) -- why Show is necessary?

triangleType :: Int -> Int -> Int -> TriangleType
triangleType a b c = triangleType' . sort $ [a,b,c]
    where
        triangleType' [x,y,z] =
        -- x <= y <= z always holds, assume x > 0
        -- x <= z ==> x < z + y
        -- y <= z ==> y < z + x
        --            z < x + y to be verified
            if z < x + y && x > 0
              then case () of
                     _ | x == z -> Equilateral -- x == y == z implied
                     _ | x == y || y == z -> Isosceles
                     _ -> Scalene
              else Illogical
        triangleType' _ = undefined -- just impossible
