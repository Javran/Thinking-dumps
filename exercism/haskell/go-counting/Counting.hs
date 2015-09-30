module Counting
  ( Color(..)
  , territories
  , territoryFor
  ) where

import qualified Data.Set as S
import Data.Array
import Data.List

data Color
  = Black
  | White
    deriving (Eq, Ord, Show)

type Coord = (Int, Int)
type Board = [ [] Char ] -- shut up hlint
type Owner = Maybe Color
type CellArray = Array Coord Owner


-- one corollary from the rule is that:
-- + two neighboring empty cells must be of the same side.
--   otherwise these two cells cannot be counted as their sides
--   in the first place.
-- so one solution is to first clustering neigboring cells to figure out
-- empty intersections, and then we can determine which side owns the
-- territory.

territories :: Board -> [(S.Set Coord, Owner)]
territories = undefined

territoryFor :: Board -> Coord -> Maybe (S.Set Coord, Owner)
territoryFor = undefined

toCellArray :: Board -> CellArray
toCellArray b = listArray ((1,1),(cols,rows)) (map toOwner (concat (transpose b)))
  where
    rows = length b
    cols = length (head b)
    toOwner ch = case ch of
        'W' -> Just White
        'B' -> Just Black
        ' ' -> Nothing
        _ -> error "invalid board"
