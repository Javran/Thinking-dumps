module Day2Hard where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Tools exposing (..)
import Mouse
import Window
import Signal
import Graphics.Element
import Debug
import Time

-- seems collage is using a different coord system
-- so we abstract out coordinate translation as a separated function
toCollageCoord : (Int, Int) -> (Int,Int) -> (Float,Float)
toCollageCoord (w,h) (x,y) = ( toFloat x - toFloat w / 2
                             , toFloat h / 2 - toFloat y )


toCollageCoordF : (Int,Int) -> (Float,Float) -> (Float,Float)
toCollageCoordF (w,h) (x,y) = ( x - toFloat w / 2
                              , toFloat h / 2 - y )

carBottom = filled black (rect 160 50)
carTop =    filled black (rect 100 60)
tire = filled red (circle 24)

drawCar = collage 300 300
  [ carBottom
  , carTop |> moveY 30
  , tire |> move (-40, -28)
  , tire |> move ( 40, -28) 
  ]

-- I'm not sure about what the exercise is asking us to do
-- if we just move cars from left to right
-- or right to left, how can we move across the bottom of the screen?
-- In this exercise I'll add speed in "y" direction, so we can eventually
-- reach the bottom of the screen,
-- and when it has reached bottom, make it reappear on top of the screen.

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
      ] |> List.map (toCollageCoordF (w,h))

type alias ScreenInfo = (Int,Int)
type alias MousePos = (Int,Int)

screenChanges : Signal (ScreenInfo,MousePos)
screenChanges = Signal.map2 (,) Window.dimensions Mouse.position

drawCarAndPaths : (ScreenInfo,MousePos) -> Graphics.Element.Element
drawCarAndPaths ((w,h),(x,y)) =
  let carForm = 
        group [ carBottom
              , carTop |> moveY 30
              , tire |> move (-40, -28)
              , tire |> move ( 40, -28) 
              ]
  in collage w h
       [ traced (solid blue) (path (pathEnds (w,h) 100))
       , carForm |> move (toCollageCoord (w,h) (100,100))
       ]

-- draw car and the path it will follow on screen
mainTest = screenChanges |> Signal.map drawCarAndPaths

getPoint : List (Float,Float) -> Float -> (Float,Float)
getPoint points percent =
  if List.length points < 2
    then Debug.crash "expecting at least 2 points in the list"
    else
      if percent > 1.0 || percent < 0.0
        then Debug.crash "invalid percent value"
        else
          let segCount = List.length points - 1
              -- find out which segment does the current "percent"
              -- belongs to, the i-th segment begins at points[i-1]
              -- and ends at points[i], so 1 <= i <= segCount
              findSeg i =
                let segEndPercent = toFloat i / toFloat segCount
                in if percent <= segEndPercent
                     then i
                     else 
                       if i > segCount
                          then Debug.crash "cannot find a segment that fits"
                          else findSeg (i+1)
              currentSeg = findSeg 1
              currentSegBeginPoint = listGet (currentSeg-1) points
              currentSegEndPoint = listGet currentSeg points
              boundedPercentRest = (toFloat currentSeg / toFloat segCount - percent)
                                   / (1 / toFloat segCount)
              boundedPercent = 1 - boundedPercentRest
          in getBetweenPoints currentSegBeginPoint currentSegEndPoint boundedPercent

getBetweenPoints : (Float,Float) -> (Float,Float) -> Float -> (Float, Float)
getBetweenPoints (beginX,beginY) (endX,endY) percent =
  if percent < 0.0 || percent > 1.0
     then Debug.crash "invalid percent value"
     else
       let dx = endX - beginX
           dy = endY - beginY
       in (beginX + dx*percent, beginY + dy*percent)

type Updates
  = UMouseMove MousePos
  | UWindowSize ScreenInfo
  | UDrawNext

type alias State =
  { screenInfo : ScreenInfo
  , mouseX : Int
  , progress : Float
  }

main =
  let signals =
        [ Signal.map UMouseMove Mouse.position
        , Signal.map UWindowSize Window.dimensions
        , Signal.map (always UDrawNext) (Time.every (Time.millisecond * 100))
        ]
      initState = 
        { screenInfo = (200,200)
        , mouseX = 0
        , progress = 0
        }
      go update state =
        case update of
          UMouseMove (x,_) -> { state | mouseX = x }
          UWindowSize si -> { state | screenInfo = si }
          UDrawNext -> 
            { state
            | progress =
                let s' = state.progress + 0.01
                in if s' <= 1.0 
                  then s'
                  else 0.0
            }
      render state =
        let (w,h) = state.screenInfo
            points = pathEnds (w,h) 100
            point = getPoint points state.progress
            carForm = 
              group [ carBottom
                    , carTop |> moveY 30
                    , tire |> move (-40, -28)
                    , tire |> move ( 40, -28) 
                    ]
        in collage w h [carForm |> move point]
  in Signal.mergeMany signals
    |> Signal.foldp go initState
    |> Signal.map render
