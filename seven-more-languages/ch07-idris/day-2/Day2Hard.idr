module Day2Hard

import Data.So

-- write a function using "So" that does not allow colors or dimensions that
-- are out of bounds
data BndInt : (l : Integer) -> (h : Integer) -> (x : Integer) -> Type where
  BI : (l : Integer) -> (h : Integer) -> (x : Integer) -> So (l <= x && x <= h) -> BndInt l h x

boundedInt : (l : Integer) -> (h : Integer) -> (x : Integer) -> Maybe (BndInt l h x)
boundedInt l h x = case choose (l <= x && x <= h) of
  Left valid => Just (BI l h x valid)
  Right _ => Nothing
