module Tools where

import Html exposing (..)

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
