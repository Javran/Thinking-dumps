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


#quit;;
