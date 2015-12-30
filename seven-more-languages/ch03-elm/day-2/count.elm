import Mouse
import Signal
import Graphics.Element

count sig = Signal.foldp (\s c-> c+1) 0 sig

main = Signal.map Graphics.Element.show (count Mouse.position)
