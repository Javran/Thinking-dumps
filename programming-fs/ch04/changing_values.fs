// note this file only works in interactive mode

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

// mutable records

open System

// mutable fields are given explicitly by "mutable" keyword
type MutableCar =
    { Make : string
    ; Model : string
    ; mutable Miles : int };;

let driveForASeason car =
    let rng = new Random()
    car.Miles <- car.Miles + rng.Next() % 10000;;

let kitt =
    { Make = "Pontiac"
    ; Model = "Trans Am"
    ; Miles = 0};;

driveForASeason kitt
driveForASeason kitt
driveForASeason kitt
driveForASeason kitt

printfn "%A" kitt;;

#quit;;
