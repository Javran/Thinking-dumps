// note this file only works in interactive mode

// converting modules to classes

// the points are:
// * abstract elements into a series of functions
//      rather than an unorganized series of values
// * next we need to make functions to abstract or simplify things
//      since it'll still take a while for others 
//      to figure out how to make these functions work together

// I think good practice can be learnt from using and real world programming.
// Skipping this part.

// intentional shadowing

// the idea is: declaring a new value with same name 
//      can shadow the previous value with that name.
// sometimes this might be beneficial, take "Checked arithmetic" as an example:

let maxInt = System.Int32.MaxValue

maxInt + 1;;
// -2147483647

open Checked

let mightFail =
    try
        maxInt + 1
    with
    | _ as ex ->
        printfn "Error: %s" ex.Message
        0

// but sometimes it might be confusing,
// since the behavior of functions would be changed sliently ...

// Controlling module usage

// use attribute: [<RequireQualifiedAccess>] to enforce using qualified import
// use attribute: [<AutoOpen>] to open a namespace automatically when its parent is imported

#quit;;
