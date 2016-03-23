module Day2Hard where

import Color exposing (..)
import Graphics.Collage exposing (..)

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
