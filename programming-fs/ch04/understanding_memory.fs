// note this file only works in interactive mode

printfn "%A" <| Unchecked.defaultof<int>;;
// 0
printfn "%A" <| typeof<int>;;
// System.Int32
printfn "%A" <| sizeof<int>;;
// 4

let isNull =
    function
       | null ->
            true
       | _ ->
            false;;

isNull "a string";;
// false
isNull (null : string);;
// true

type Thing =
    | Planet
    | Animal
    | Mineral

// line below would fail
//(null : Thing);;

// aliasing
let x = [| 0 |]
let y = x;;

x.[0] <- 3;;

printfn "%A %A" x y;;
// all changes to [|3|]

y.[0] <- 1;;
printfn "%A %A" x y;;
// all changes to [|1|]

// "mutable variable" ... I think two words are essentially the same... anyway
// to change contents of a mutable value, use "<-"

let mutable message = "Boat";;

printfn "Nice %s" message;;
// Nice Boat!

message <- "Universe";;
printfn "Nice %s" message;;
// Nice Universe!
// that's why they are called "mutable" :)

let invalidUseOfMutable () =
    let mutable x = 0
    // following line will fail, mutable values cannot be captured in a closure
    //let incrementX() = x <- x + 1
    // incrementX()
    x;;

// reference cells

let planets =
    ref [
        "Mercury";  "Venus";    "Earth";
        "Mars";     "Jupiter";  "Saturn";
        "Uranus";   "Neptune";  "Pluto"
    ];;

// think "!" something like de-reference
planets := ! planets |> List.filter (fun p -> p <> "Pluto")

Seq.iter (printfn "%A") <| !planets;;

// we have "decr" and "incr" to simply modify contents inside int ref
let x = ref 0;;

incr x;;
incr x;;
decr x;;
printfn "%A" !x;;
// 1

// mutable records ... TODO

#quit;;
