module Problem99 where

import qualified Data.Array as Arr
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M

import Data.Foldable

type Words = IM.IntMap [String]

type Coord = (Int,Int)

-- site
data Dir = DV | DH -- vertical or horizontal
data Site = Site Int Dir

-- at most 2 sites on the same coord (one v and one h)
type Framework = (M.Map Coord [Site], Arr.Array Coord (Maybe Char))

data Crossword = CW Words Framework

mkWords :: [String] -> Words
mkWords = foldr update IM.empty
  where
    update w = IM.alter ins lw
      where
        lw = length w
        ins Nothing = Just [w]
        ins (Just xs) = Just (w:xs)

mkFramework :: [String] -> Framework
mkFramework [] = error "empty input"
mkFramework xs@(y:ys)
    | y == [] = error "first line empty"
    | allLengthEqual = undefined -- TODO
    | otherwise = error "inconsistent length"
  where
    allLengthEqual = all (lengthEq y) ys

    nCols = length y
    nRows = length xs

    rect =
        Arr.array
          ((1,1),(nRows,nCols))
          (zip [(r,c) | r<-[1..nRows], c<-[1..nCols]]
               (concat xs))

lengthEq :: [a] -> [b] -> Bool
lengthEq [] [] = True
lengthEq (_:as) (_:bs) = lengthEq as bs
lengthEq _ _ = False
