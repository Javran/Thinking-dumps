import Mouse
import Signal
import Graphics.Element
import Tools exposing (..)

-- signals are fired when something happens ...
-- to handle all possibilities we need a sum type
type Update 
  = MousePosition (Int, Int)
  | MouseDown Bool

type alias State =
  { isDown : Bool
  , y : Int
  }

-- see example: http://package.elm-lang.org/packages/elm-lang/core/2.0.1/Signal#merge
-- here we need to know mouse positions and whether left mouse button is down
-- (unfortunately I can't find the signal that deals with other mouse buttons,
-- but it makes little different in terms of how it is working)
updates = 
  Signal.merge 
    (Signal.map MousePosition Mouse.position)
    (Signal.map MouseDown Mouse.isDown)

-- on every click, we sample mouse position and render y value on the screen
main =
  let 
    initState = { isDown=False, y=0 }
    onUpdate u state =
      let
        {isDown,y} = state
      in case u of
           MousePosition (_,y') -> { state | y = if isDown then y' else y }
           MouseDown newD -> { state | isDown = newD }
  in Signal.foldp onUpdate initState updates
     |> Signal.map (asDiv << .y)
