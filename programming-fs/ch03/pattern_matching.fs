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

open System

let highLowGame () =
    let rng = new Random()
    let secretNumber = rng.Next() % 100

    let rec highLowGameStep () =

        printfn "Guess the secret number:"
        let guessStr = Console.ReadLine()
        
        let guess = Int32.Parse(guessStr)

        match guess with
        | _ when guess > secretNumber
            ->
                printfn "The secret number is lower."
                highLowGameStep()
        | _ when guess = secretNumber
            ->
                printfn "You've guessed correctly."
                ()
        | _ when guess < secretNumber
            ->
                printfn "The secret number is higher."
                highLowGameStep()
        // fallback case to eliminate warnings
        | _ -> ()

    highLowGameStep();;
        
// uncomment to play the game
// highLowGame();;

let elem e ls =
    Seq.exists ((=) e) ls;;

let vowelTest1 c =
    match c with
    | 'a' | 'e' | 'i' | 'o' | 'u'
        -> true
    | _ -> false;;

Seq.iter ( printfn "%b" << vowelTest1 ) "abcde";;
// t f f f t 

let vowelTest2 c =
    match c with
    | _ when elem c "aeiou"
        -> true
    | _ -> false;;

Seq.iter ( printfn "%b" << vowelTest2 ) "abcde";;
// t f f f t 

let describeNumbers x y =
    match x,y with
    | 1, _
    | _, 1
        -> "One of the numbers is one."
    // simply replacing the line below with "| (2,2)" will do as well
    | (2, _) & (_, 2)
        -> "Both of the numbers are two."
    | _ -> "Other."

#quit;;
