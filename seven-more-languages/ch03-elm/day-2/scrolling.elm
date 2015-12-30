import Mouse
import Window
import Signal
import Graphics.Element

div a b = Graphics.Element.show (toFloat a / toFloat b)

main = Signal.map2 div Mouse.y Window.height
