module Connect
  ( resultFor
  , Color(..)
  ) where

data Color = Black | White

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



resultFor :: [String] -> Maybe Color
resultFor = undefined
