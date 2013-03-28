// note this file only works in interactive mode

// use keyword 'match' and 'with' to do pattern matching

let describeNumber x =
    let isOdd x = (x%2 = 1)

    (match isOdd x with
    | true ->
        printfn "%d is odd"
    | false ->
        printfn "%d is even")  x;;

List.iter describeNumber [1..5];;
// 1 is odd
// 2 is even
// ...

// wildcard '_' that matches everything
let testAnd x y =
    match x, y with
    | true, true -> true
    | _, _ -> false;;

testAnd true true;;
// true
testAnd false true;;
// false

// named patterns
let greet name =
    printfn "Hello, %s" <|
        match name with
        | "Robert" ->
            "Bob"
        | "William" ->
            "Bill"
        | name -> name;;

List.iter greet ["Robert"; "Javran"; "William"];;

// matching literals

let bill = "Bill Gates"
let greet1 name =
    match name with
    | bill -> "Hello Bill!"
    // we'll get a warning here
    //     because bill cannot be used as a literal value
    | x -> sprintf "Hello, %s" x;;

// this is a constant
// any literal value marked "Literal" can be used inside of a pattern match
// note: Begin With A Capital !
[<Literal>]
let Bill = "Bill Gates";;

let greet3 name =
    match name with
    | Bill -> "Hello Bill!"
    | x -> sprintf "Hello, %s" x;;

List.map greet3 ["Bill G."; "Bill Gates"; Bill];;

#quit;;
