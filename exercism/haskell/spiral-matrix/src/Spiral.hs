module Spiral
  ( spiral
  )
where

import Data.Function
import Data.List

type Coord = (Int, Int)

newtype Dir = Dir (Int, Int)

up, down, left, right :: Dir
[up, down, left, right] =
  fmap Dir [(-1, 0), (1, 0), (0, -1), (0, 1)]

applyDir :: Dir -> Coord -> Coord
applyDir (Dir (dr, dc)) (r, c) = (dr + r, dc + c)

spiral :: Int -> [[Int]]
spiral 0 = []
spiral n =
  (fmap . fmap) fst -- remove coords
    . groupBy ((==) `on` fst . snd) -- group base on r.
    $ sortOn snd pairs -- sorting on (r,c) produces row-majority order
  where
    -- zip tile numbers with coordinates
    pairs = zip [1 ..] (spiralCoords n (0, 0))

spiralCoords :: Int -> Coord -> [Coord]
spiralCoords n coord =
  scanl (flip applyDir) coord
    . intercalate [right]
    $ fmap spiralAround [n, n -2 .. 1]

-- Dir to form one layer of sprial around a square of size n.
spiralAround :: Int -> [Dir]
spiralAround n =
  replicate (n -1) right
    <> replicate (n -1) down
    <> replicate (n -1) left
    <> replicate (n -2) up
