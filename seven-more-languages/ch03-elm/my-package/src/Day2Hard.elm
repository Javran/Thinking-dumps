module Day2Hard where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Tools exposing (..)

-- seems collage is using a different coord system
-- so we abstract out coordinate translation as a separated function
toCollageCoord : (Int, Int) -> (Int,Int) -> (Float,Float)
toCollageCoord (w,h) (x,y) = ( toFloat x - toFloat w / 2
                             , toFloat h / 2 - toFloat y )

carBottom = filled black (rect 160 50)
carTop =    filled black (rect 100 60)
tire = filled red (circle 24)

drawCar = collage 300 300
  [ carBottom
  , carTop |> moveY 30
  , tire |> move (-40, -28)
  , tire |> move ( 40, -28) 
  ]

-- for now I'm not sure about
-- what the exercise is asking us to do
-- if we just move cars from left to right
-- or right to left, how can we move across the bottom of the screen?
-- TODO: make it move from left to right as a constant speed
-- add speed in "y" direction, so we can eventually
-- reach the bottom of the screen
-- might need to wrap it so to make it reappear on top of the screen.


-- pathEnds (w,h) p calculates
-- 5 important points for determining cars' path
--
--     0-------------------+
--     |                   |
--     +-------------------1
--     |                   |
--     2-------------------+
--     |                   |
--     +-------------------3
--     |                   |
--     4-------------------+
--
-- the car will follow the straight line indicated by:
-- 0->1, 1->2, 2->3, 3->4
-- top-left corner begins at (p,p)
-- and ends at botton-right corner (w-p,h-p)
-- so "p" serves as a parameter for padding
pathEnds : (Int, Int) -> Int -> List (Float,Float)
pathEnds (w,h) pI =
  let p = toFloat pI
      w' = toFloat (w-pI-pI)
      h' = toFloat (h-pI-pI)
  in  [ (p,p), (p+w',p+h'/4)
      , (p,p+h'/2), (p+w', p+h'*3/4)
      , (p,p+h')
      ]

main = asDiv (pathEnds (400,300) 10)
