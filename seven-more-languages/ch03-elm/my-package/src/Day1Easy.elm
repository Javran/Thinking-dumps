module Day1Easy where

import Html exposing (..)
import List

product : List number -> number
product xs =
  case xs of
    [] -> 1 
    hd :: tl -> hd * product tl

day1Easy =
  div [] (List.map (\x -> div [] [text x])
            [ "Hello"
            , "second line"
            , "excited"
            , "product of 2,3,5,7: " ++ toString (product [2,3,5,7])
            ])
