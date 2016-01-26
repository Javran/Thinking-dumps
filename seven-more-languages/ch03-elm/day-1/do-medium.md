## Write a function called multiply

I guess there might be something missing, but anyway ...

```
> multiply = (*)
<function> : number -> number -> number
```

## Use currying to express 6*8

```
> ((*) 6) 8
48 : number
```

## Make a list of person, and find all older than 16

```
> mkPerson n a = { name = n, age = a, address = "unspecified" }
<function> : a -> b -> { address : String, age : b, name : a }
> persons = [ mkPerson "p1" 10, mkPerson "p2" 16, mkPerson "p3" 20 ]
[{ name = "p1", age = 10, address = "unspecified" },{ name = "p2", age = 16, address = "unspecified" },{ name = "p3", age = 20, address = "unspecified" }]
    : List { address : String, age : number, name : String }
> List.filter
<function> : (a -> Bool) -> List a -> List a
> List.filter (\x -> x.age > 16) persons
[{ name = "p3", age = 20, address = "unspecified" }]
    : List { address : String, name : String, age : number }
```

