{-# LANGUAGE LambdaCase #-}
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
    modifyBearing f = modify (& _1 %~ f)
    modifyCoord f = modify (& _2 %~ f)
    interpret :: Char -> State Robot ()
    interpret c = case c of
        'L' -> modifyBearing turnLeft
        'R' -> modifyBearing turnRight
        'A' -> advance
        _ -> error "unknown command"
    advance :: State Robot ()
    advance = gets bearing >>= \case
        North -> modifyCoord (& _2 %~ succ)
        South -> modifyCoord (& _2 %~ pred)
        East  -> modifyCoord (& _1 %~ succ)
        West  -> modifyCoord (& _1 %~ pred)
