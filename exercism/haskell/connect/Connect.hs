module Connect
  ( resultFor
  , Color(..)
  ) where

import qualified Data.Set as S
import Data.Array
import Data.Foldable

{-
  first of all we assume there should at most be one winner
  because paths got to have intersections so it will be impossible
  to connect 2 opposite sides at the same time.

  the gameboard is different from other games because one cell might
  might have up to 6 neighborhoods. for example, consider the following board:

  . O . X .
   . X X O .
    O O ? X .
     . X O X O
      X O O O X

  cell '?' has 6 neighboring cells.

  we would like to implement a search algorthm, visiting from one side
  and spreading to find out if the oppsite side is reachable.
  then we perform this search twice: one searches from top to bottom
  and another searches from left to right. if any of these two searches
  have reached its opposite side, we know immediately who is the winner
  otherwise there is no winner.

-}

data Color = Black | White deriving (Eq, Show)

type Coord = (Int, Int)
type GameBoard = Array Coord (Maybe Color)

-- | calculate neighbor coordinates, note that bounds are ignored
--   so there might be invalid coordinates in results
neighborCoords :: Coord -> [Coord]
neighborCoords (x,y) =
    [            (x,y-1), (x+1,y+1)
    , (x-1,y  ),          (x+1,y  )
    , (x-1,y-1), (x,y+1)           ]

-- INVARIANT: coords in todoSet must be vaild (within range, color matches)
search :: GameBoard -> Color -> S.Set Coord -> S.Set Coord -> S.Set Coord
search board color todoSet visitedSet
    | S.null todoSet = visitedSet
    | otherwise =
        let newVisitedSet = visitedSet `S.union` todoSet
            expandedSet = S.filter ((== Just color) . (board !))
                        . S.filter (inRange (bounds board))
                        . foldMap (S.fromList . neighborCoords)
                        $ todoSet
            freshCoords = expandedSet `S.difference` newVisitedSet
        in search board color freshCoords newVisitedSet

resultFor :: [String] -> Maybe Color
resultFor = undefined
