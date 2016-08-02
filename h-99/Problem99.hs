module Problem99 where

import qualified Data.Array as Arr
import qualified Data.IntMap.Strict as IM

type Words = IM.IntMap [String]

type Coord = (Int,Int)
type Framework = Arr.Array Coord Char

data Crossword = CW Words Framework

fromWordList :: [String] -> Words
fromWordList = foldr update IM.empty
  where
    update w = IM.alter ins lw
      where
        lw = length w
        ins Nothing = Just [w]
        ins (Just xs) = Just (w:xs)
