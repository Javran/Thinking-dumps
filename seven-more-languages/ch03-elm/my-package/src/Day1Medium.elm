module Day1Medium where

import List
import Tools exposing (..)

multiply : number -> number -> number
multiply = (*)

day1Medium =
  let mkPerson n a = { name = n, age = a, address = "unspecified" }
      persons = [ mkPerson "p1" 10, mkPerson "p2" 16, mkPerson "p3" 20 ]
  in dayNpartX 1 "medium"
       (divConcat
          [ descAndResult "define mutiple" multiply
          , descAndResult "use currying to express 6*8" ((multiply 6) 8)
          , descAndResult "a list of person" persons
          , descAndResult "find all person in the list older than 16"
                          (List.filter (\x -> x.age > 16) persons)
          ])
