import Mouse
import Signal
import Graphics.Element

main = Signal.map Graphics.Element.show Mouse.position
