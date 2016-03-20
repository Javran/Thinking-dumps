module Day1Hard where

import Maybe
import List
import Tools exposing (..)

{-
Use `Maybe`:

http://package.elm-lang.org/packages/elm-lang/core/3.0.0/Maybe

But I find this module does not have convenient combinators like
`isJust`, making the implemtation a little bit hard to read
-}

day1Hard =
  let mkPerson n a = { name = n, age = a, address = "unspecified" }
      persons = [ mkPerson "p1" (Just 10)
                , mkPerson "p2" (Just 16)
                , mkPerson "p3" (Just 20)
                , mkPerson "p4" Nothing
                ]
      greaterThanAge age person = 
        Maybe.withDefault False (Maybe.map (\a -> a > age) person.age)
  in dayNpartX 1 "hard"
       (divConcat
          [ descAndResult "older than 16, with Maybe support"
              (List.filter (greaterThanAge 16) persons)
          ])
