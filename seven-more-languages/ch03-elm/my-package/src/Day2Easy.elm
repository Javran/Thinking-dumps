import Mouse
import Signal
import Graphics.Element
import Tools exposing (..)

-- hm, we might have to compile this file separately
-- as the type of Main has been changed

-- signals are fired when something happens ...
-- to handle all possibilities we need a sum type
type Update 
  = MousePosition (Int, Int)
  | MouseDown Bool

-- the global state, on the contrary, is a product type
-- because we need to somehow accumuate or summarize
-- all kinds of events
type State = St (Int,Int) Bool

-- see example: http://package.elm-lang.org/packages/elm-lang/core/2.0.1/Signal#merge
-- here we need to know mouse positions and whether left mouse button is down
-- (unfortunately I can't find the signal that deals with other mouse buttons,
-- but it makes little different in terms of how it is working)
updates = 
  Signal.merge 
    (Signal.map MousePosition Mouse.position)
    (Signal.map MouseDown Mouse.isDown)

-- foldp allows us to "accumuate values in the the (probably infinite) future
-- all we need is to provide a function to say what should happen
-- to the state when a new event has arrived
main =
  let onUpdate u (St p d) = case u of
        MousePosition newP -> St newP d
        MouseDown newD -> St p newD
  in Signal.foldp onUpdate (St (0,0) False) updates
     -- finally, we need to print current state out
     |> Signal.map (\x -> asDiv x)
