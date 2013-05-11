// note this file only works in interactive mode

let divide x y =
    if y = 0
        then failwithf "Cannot divide %d by zero." x
        else x / y;;

// the following line would cause error
// divide 10 0;;

let divide2 x y =
    if y = 0
        then raise <| new System.DivideByZeroException()
        else x / y;;

// the following line would cause error
// divide2 10 0;;

// handling exceptions

// try-catch expressions

open System.IO

// I don't think we need to verify if the dir / drive really exists
// the exception throw by FileInfo constructor and the member "Exists" in FileInfo 
//     contains things we needed to analyze the error
let getFileInfo filePath = 
    try
        printfn "Trying to gather information about file:"
        printfn "%s" filePath

        (*
        let matchingDrive =
            Directory.GetLogicalDrives()
            |> Array.tryFind (fun drivePath -> drivePath.[0] = filePath.[0])

        if matchingDrive = None
            then raise <| new DriveNotFoundException(filePath)

        let directory = Path.GetPathRoot( filePath )

        if not <| Directory.Exists( directory )
            then raise <| new DirectoryNotFoundException(filePath)

        if not <| File.Exists( filePath )
            then raise <| new FileNotFoundException(filePath)
        *)

        let fileInfo = new FileInfo(filePath)
        if not fileInfo.Exists
            then raise <| new FileNotFoundException(filePath)
        printfn "Created = %s" <| fileInfo.CreationTime.ToString()
        printfn "Access = %s" <| fileInfo.LastAccessTime.ToString()
        printfn "Size = %d" fileInfo.Length

        0
    with
    | :? DriveNotFoundException
    | :? DirectoryNotFoundException ->
        printfn "Unhandled Drive or Directory not found exception"
        1
    | :? FileNotFoundException as ex ->
        printfn "Unhandled FileNotFoundException: %s" ex.Message
        3
    | :? IOException as ex ->
        printfn "Unhandled IOException: %s" ex.Message
        4
    | _ as ex ->
        printfn "Unhandled Exception: %s" ex.Message
        5

// ":?" is call a "dynamic type test operator", does what its name suggests

[ @"X:\NoSuchDrive\a"
; @"D:\NoSuchDir\a"
; @"D:\tmp\NoSuchFile"
; @"D:\tmp\file.txt"]
    |> Seq.iter (getFileInfo >> ignore);;

// try-finally

let tryFinallyTest() =
    try
        printfn "Before exception ..."
        failwith "ERROR!"
        printfn "After exception raised ..."
    finally
        printfn "Finally block executing ..."

let test() =
    try
        tryFinallyTest()
    with
    | ex -> printfn "Exception: %s" ex.Message;;

test();;
// note: there is no "try-catch-finally" expression in F# ...

// reraising exceptions
let tryWithBackOff f times =
    let mutable attempt = 1
    let mutable success = false

    while not success do
        try
            printfn "Try to run the function ..."
            f()
            success <- true
        with ex ->
            attempt <- attempt + 1
            if attempt >= times then
                reraise()

let errorFunction _ =
    raise <| new System.Exception("Test Exception")
    ();;

try
    tryWithBackOff errorFunction 5
with ex ->
    printfn "tryWithBackOff failed for %d time, because: %s" 5 ex.Message

// defining exceptions

// F# provides some lightweight exception syntax

open System
open System.Collections.Generic

exception ErrorWithoutArg
exception ErrorWithReason of string
exception ErrorWithTuple of int * int

let raiseSpecialError errId =
    try
        match errId with
        | 0 -> raise <| ErrorWithoutArg
        | 1 -> raise <| ErrorWithReason "Test error"
        | 2 -> raise <| ErrorWithTuple (123,456)
        | _ -> ()
    with
    | ErrorWithoutArg ->
        printfn "Error without argument"
    | ErrorWithReason(r) ->
        printfn "Error with reason: %s" r
    | ErrorWithTuple(a,b) ->
        printfn "Error with a tuple: %A" (a,b)
    
raiseSpecialError 0
raiseSpecialError 1
raiseSpecialError 2


#quit;;
