module Day2Medium where

import Tools exposing (..)
import Time
import Signal
import Mouse exposing (..)
import Window exposing (..)
import List

import Graphics.Collage exposing (..)
import Color exposing (..)

toCollageCoord : (Int, Int) -> (Int,Int) -> (Float,Float)
toCollageCoord (w,h) (x,y) = ( toFloat x - toFloat w / 2
                             , toFloat h / 2 - toFloat y )

drawShape (w,h) (x,y) =
  collage w h 
            [ filled black (ngon 5 40)
              |> move (toCollageCoord (w,h) (x,y))
            ]

allForms : List Form
allForms = 
  [ red, green, blue ]
  |> List.concatMap 
     (\color ->
        [ square 40
        , circle 40
        , ngon 5 40
        , ngon 3 40
        ] |> List.concatMap (\shape -> [ filled color shape ]))

type Update
  = PositionUpdate (Int,Int) (Int,Int)
  | NextPicture Bool

main = 
  let drawSignal = 
        Signal.map2 (\w p -> PositionUpdate w p) 
          Window.dimensions
          Mouse.position
      onUpdate s (((w,h),(x,y)),styInd) =
        case s of
          PositionUpdate newW newP -> ((newW,newP),styInd)
          NextPicture b -> (((w,h),(x,y)), if b then styInd+1 else styInd)
  in Signal.merge 
       drawSignal
       (Mouse.isDown |> Signal.map NextPicture)
     |> Signal.foldp onUpdate (((0,0),(0,0)),0)
     |> Signal.map asDiv

-- counts up from zero with one count per second
main2 =
  Time.every Time.second
  |> Signal.foldp (\_ s -> s + 1) 0
  |> Signal.map asDiv
