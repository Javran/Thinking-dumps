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

-- CV for "color value"
CV : Type
CV = BndInt 0 255

data Color : Type where
  RGB : (r : CV) -> (g : CV) -> (b : CV) -> Color

makeColor : Integer -> Integer -> Integer -> Maybe Color
makeColor r g b = RGB <$> boundedInt r <*> boundedInt g <*> boundedInt b

-- DV for "dimension value"
-- the exercise does not say anything about the bound
-- so we just make our own: values in range [0..65535] are all valid
DV : Type
DV = BndInt 0 65535

data Dimension : Type where
  Dim2 : (x : DV) -> (y : DV) -> Dimension
  
makeDimension : Integer -> Integer -> Maybe Dimension
makeDimension a b = Dim2 <$> boundedInt a <*> boundedInt b
