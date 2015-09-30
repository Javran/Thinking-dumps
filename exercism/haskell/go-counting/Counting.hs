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

territories :: Board -> [(S.Set Coord, Maybe Color)]
territories = undefined

territoryFor :: Board -> Coord -> Maybe (S.Set Coord, Maybe Color)
territoryFor = undefined
