import Mouse
import Signal
import Graphics.Element
import Tools exposing (..)

-- hm, we might have to compile this file separately
-- as the type of Main has been changed

type Update 
  = MousePosition (Int, Int)
  | MouseDown Bool

type State = St (Int,Int) Bool

updates = 
  Signal.merge 
    (Signal.map MousePosition Mouse.position)
    (Signal.map MouseDown Mouse.isDown)

main =
  let onUpdate u (St p d) = case u of
        MousePosition newP -> St newP d
        MouseDown newD -> St p newD
  in Signal.foldp onUpdate (St (0,0) False) updates
     |> Signal.map (\x -> asDiv x)
