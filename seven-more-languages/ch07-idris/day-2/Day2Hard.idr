module Day2Hard

import Data.So

-- write a function using "So" that does not allow colors or dimensions that
-- are out of bounds

data BndInt : (l : Integer) -> (h : Integer) -> Type where
  BI : (x : Integer) -> So (l <= x && x <= h) -> BndInt l h

boundedInt : (x : Integer) -> Maybe (BndInt l h)
boundedInt x = case choose (l <= x && x <= h) of
  Left valid => Just (BI x valid)
  Right _ => Nothing

v1 : Maybe (BndInt 1 10)
v1 = boundedInt 9 -- Just ..

v2 : Maybe (BndInt 1 10)
v2 = boundedInt 11 -- Nothing
