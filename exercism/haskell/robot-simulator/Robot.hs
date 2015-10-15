module Robot
  ( Bearing(..)
  , Robot
  , turnLeft
  , turnRight
  , mkRobot, bearing, coordinates
  , simulate
  ) where

import Control.Monad.State
import Control.Lens

data Bearing = North | East | South | West
  deriving (Enum, Show, Eq)

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

simulate :: Robot -> String -> Robot
simulate r xs = execState (mapM_ interpret xs) r
  where
    interpret :: Char -> State Robot ()
    interpret c = case c of
        'L' -> modify (& _1 %~ turnLeft)
        'R' -> modify (& _1 %~ turnRight)
        'A' -> advance
        _ -> error "unknown command"
    advance :: State Robot ()
    advance = do
        (b,(x,y)) <- get
        case b of
            North -> put (b,(x,y+1))
            South -> put (b,(x,y-1))
            East  -> put (b,(x+1,y))
            West  -> put (b,(x-1,y))

