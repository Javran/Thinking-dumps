module Problem91Test where

import Problem91
import qualified Data.Set as S

-- 2 coordinates are near if and only if
-- there is a direct knight move from one to the other.
near :: Coord -> Coord -> Bool
near (x1,y1) (x2,y2) = dist == 3 && dx /= 0 && dy /= 0
  where
    dx = abs (x1-x2)
    dy = abs (y1-y2)
    dist = dx + dy

validatePath :: [Coord] -> Bool
validatePath xs = and $ zipWith near xs (tail xs)

validateTour :: Int -> [Coord] -> Bool
validateTour n xs = validatePath xs && S.fromList xs == allCells
  where
    cs = [1..n]
    allCells = S.fromList [(x,y) | x <- cs, y <- cs]
