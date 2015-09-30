module Counting
  ( Color(..)
  , territories
  , territoryFor
  ) where

import qualified Data.Set as S

data Color
  = Black
  | White
    deriving (Eq, Ord, Show)

type Coord = (Int, Int)

type Board = [ [] Char ] -- shut up hlint

-- one corollary from the rule is that:
-- + two neighboring empty cells must be of the same side.
--   otherwise these two cells cannot be counted as their sides
--   in the first place.
-- so one solution is to first clustering neigboring cells to figure out
-- empty intersections, and then we can determine which side owns the
-- territory.

territories :: Board -> [(S.Set Coord, Maybe Color)]
territories = undefined

territoryFor :: Board -> Coord -> Maybe (S.Set Coord, Maybe Color)
territoryFor = undefined

