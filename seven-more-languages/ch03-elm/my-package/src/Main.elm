import Html exposing (..)
import List

main =
  div [] (List.map (\x -> div [] [text x])
            ["Hello", "second line", "excited"])
