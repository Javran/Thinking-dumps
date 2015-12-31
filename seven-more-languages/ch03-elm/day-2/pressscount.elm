import Keyboard
import Signal
import Graphics.Element

main = Signal.map Graphics.Element.show (Signal.foldp (\ d p -> p + d.x ) 0 Keyboard.arrows)
