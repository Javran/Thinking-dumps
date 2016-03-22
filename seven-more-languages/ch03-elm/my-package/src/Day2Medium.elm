module Day2Medium where

import Tools exposing (..)
import Time
import Signal

-- counts up from zero with one count per second
main =
  Time.every Time.second
  |> Signal.foldp (\_ s -> s + 1) 0
  |> Signal.map asDiv
