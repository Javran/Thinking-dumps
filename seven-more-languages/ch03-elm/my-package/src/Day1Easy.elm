module Day1Easy where

import Html exposing (..)
import List
import Tools exposing (..)

product : List number -> number
product xs =
  case xs of
    [] -> 1 
    hd :: tl -> hd * product tl


day1Easy =
  let sample = [ { x = 1 }, { x = 3 }, { x = 6 } ]
      person =
        { name= "Person"
        , age= 20
        , address= 
            { line1= "line1"
            , line2= "line2"
            , city= "city"
            ,  zipCode= "127001"
            }
        }
  in 
    dayNpartX 1 "easy"
      (divConcat 
         [ descAndResult "product of 2,3,5,7" (product [2,3,5,7])
         , descAndResult "point records" sample
         , descAndResult "value of x" (List.map .x sample)
         , descAndResult "describe a person" person
         ])
