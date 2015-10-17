module Connect
  ( resultFor
  , Color(..)
  ) where

data Color = Black | White

-- first of all we assume there should at most be one winner
-- because paths got to have intersections

resultFor :: [String] -> Maybe Color
resultFor = undefined
