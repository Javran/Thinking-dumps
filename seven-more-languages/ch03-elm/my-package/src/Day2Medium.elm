module Day2Medium where

import Tools exposing (..)
import Time
import Signal
import Mouse exposing (..)
import Window exposing (..)

import Graphics.Collage exposing (..)
import Color exposing (..)

toCollageCoord : (Int, Int) -> (Int,Int) -> (Float,Float)
toCollageCoord (w,h) (x,y) = ( toFloat x - toFloat w / 2
                             , toFloat h / 2 - toFloat y )

drawShape (w,h) (x,y) =
  collage w h 
            [ filled black (rect 10 20)
              |> move (toCollageCoord (w,h) (x,y))
            ]

main =
  Signal.map2 drawShape Window.dimensions Mouse.position

-- counts up from zero with one count per second
main2 =
  Time.every Time.second
  |> Signal.foldp (\_ s -> s + 1) 0
  |> Signal.map asDiv
