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

let testXor x y =
    match x, y with
    | a,b when a <> b
        -> true
    | _ -> false;;

List.iter ( printfn "%b" << (fun (a,b) -> testXor a b) ) [
    (true,true);
    (true,false);
    (false,true);
    (false,false);];;

// pattern matching against list
// looks stupid, but come on, it's merely an example after all :)
let rec listLength l =
    match l with
    | []
        -> 0
    | [_]
        -> 1
    | [_; _]
        -> 2
    | [_; _; _]
        -> 3
    | head::tail
        -> 1 + listLength tail;;

List.iter ( printfn "%d" << listLength )
    [ []; [1]; [1;2]; [for i in 1 .. 100 do yield i]];;
// 0 1 2 100

let describeOption o =
    match o with
    | Some(42)
        -> "The answer was 42, but what was the question?"
    | Some(x)
        -> sprintf "The answer was %d" x
    | None
        -> "No answer found.";;

List.iter ( printfn "%s" << describeOption )
    [ Some(1); Some(2); Some(42); None ];;


// uncomment the line below and an error saying "incomplete pattern match" will emerge
// let 1 = 2;;

let addOptionValues = fun (Some(x), Some(y)) -> x+y;;

addOptionValues (Some(10),Some(20));;
// 30

// wildcard pattern: '_'

List.iter (fun _ -> printfn "Step...") [1..3];;
// we don't care about the argument in this case, so we use a wildcard to ignore the arg

let _, second, _ = (1,2,3);;
printfn "The second element is %d" second;;

// test if '::' in F# can achieve things just like ':' in Haskell achieves
let a :: b :: xs = [1..10];;
printfn "first: %d, second: %d, rest: %A" a b xs;;
// ok, it works
// output: f: 1, s: 2, rest: [3..10]

// alternate lambda syntax
let rec listLength theList =
    match theList with
    | []        -> 0
    | [_]       -> 1
    | [_;_]     -> 2
    | [_;_;_]   -> 3
    | hd::tail  -> 1 + listLength tail

printfn "len: %d" <| listLength [1..10];;
// 10

let rec funListLength = 
    function
    | []        -> 0
    | [_]       -> 1
    | hd::tail  -> 1 + funListLength tail;;

printfn "len: %d" <| listLength [1..100];;
// 100

#quit;;
