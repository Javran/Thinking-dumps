Use `Maybe`:

[Maybe doc](http://package.elm-lang.org/packages/elm-lang/core/3.0.0/Maybe)

But I find this module does not have convenient combinators like
`isJust`, making the implemtation a little bit hard to read

```
> mkPerson n a = { name = n, age = a, address = "unspecified" }
<function> : a -> b -> { address : String, age : b, name : a }
> persons = [ mkPerson "p1" (Just 10), mkPerson "p2" (Just 16) , mkPerson "p3" (Just 20), mkPerson "p4" Nothing ]
[{ name = "p1", age = Just 10, address = "unspecified" },{ name = "p2", age = Just 16, address = "unspecified" },{ name = "p3", age = Just 20, address = "unspecified" },{ name = "p4", age = Nothing, address = "unspecified" }]
    : List { address : String, age : Maybe.Maybe number, name : String }
> \p -> Maybe.withDefault False (Maybe.map (\a -> a > 16) p.age)
<function> : { a | age : Maybe.Maybe number } -> Bool
> List.filter ( \p -> Maybe.withDefault False (Maybe.map (\a -> a > 16) p.age) ) persons
[{ name = "p3", age = Just 20, address = "unspecified" }]
    : List { address : String, name : String, age : Maybe.Maybe number }
```
