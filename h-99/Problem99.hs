module Problem99 where

import qualified Data.Array as Arr
import qualified Data.IntMap.Strict as IM

type Words = IM.IntMap [String]

type Coord = (Int,Int)
type Framework = Arr.Array Coord Char

data Crossword = CW Words Framework

-- fromRawContent :: String -> _
