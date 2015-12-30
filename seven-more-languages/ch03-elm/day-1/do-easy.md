For now we have not yet figured out how to write source code,
so just playing with REPL and write code in this text file.

(1)
```
> product xs = case xs of \
|   [] -> 1 \
|   hd :: tl -> hd * product tl
<function> : List number -> number
> product [2,3,5,7]
210 : number
```

(2)

```
> List.map .x [ { x = 1 }, { x = 3 }, { x = 6 } ]
[1,3,6] : List number
```

(3)

```
{ name: String
, age: number
, address: {
    line1: String
    line2: String
    city: String
    zipCode: String
    } }
```

(4)

In general, use ADT when you want to take advantage of the type system.
or use record if you want friendly interface to JavaScript.
I don't think any of them is obviously "easier" than the others without
taking the actual use case into account.
