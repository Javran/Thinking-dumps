import Keyboard
import Signal
import Graphics.Element

main = Signal.map Graphics.Element.show Keyboard.arrows
