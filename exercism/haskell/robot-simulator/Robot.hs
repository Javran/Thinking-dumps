module Robot
  ( Bearing(..)
  , Robot
  , turnLeft
  , turnRight
  , mkRobot, bearing, coordinates
  ) where

data Bearing = North | East | South | West
  deriving (Enum, Show)

turnLeft, turnRight :: Bearing -> Bearing

turnLeft  = toEnum . (`rem` 4) . (+ 3) . fromEnum
turnRight = toEnum . (`rem` 4) . (+ 1) . fromEnum

type Robot = (Bearing, (Int, Int))

mkRobot :: Bearing -> (Int, Int) -> Robot
mkRobot = (,)

bearing :: Robot -> Bearing
bearing = fst

coordinates :: Robot -> (Int, Int)
coordinates = snd
