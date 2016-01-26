import Mouse
import Signal
import Graphics.Element

main = Signal.map Graphics.Element.show (Signal.sampleOn Mouse.clicks Mouse.position)
