module Tools where

import Html exposing (..)
import List
import Debug

divText : String -> Html
divText s = div [] [text s]

-- convert arbitrary value to "div" object
asDiv : a -> Html
asDiv = toString >> divText

day : number -> Html
day = asDiv

divConcat : List Html -> Html
divConcat = div []

dayNpartX : number -> String -> Html -> Html
dayNpartX n x html = 
  let title = "day " ++ toString n ++ " " ++ x
  in divConcat [ h1 [] [divText title]
               , html
               ]

descAndResult : String -> result -> Html
descAndResult desc r =
  divConcat [ h2 [] [divText desc]
            , asDiv r
            ]

listGet : Int -> List a -> a
listGet n xs = 
  case n of
    0 -> case xs of
           [] -> Debug.crash "empty list"
           (h :: _) -> h
    _ -> if n < 0
           then Debug.crash "invalid index"
           else case xs of
                  -- here we know that n /= 0, but the
                  -- list is already empty
                  [] -> Debug.crash "empty list"
                  (_::t) -> listGet (n-1) t
