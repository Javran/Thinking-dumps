// note this file only works in interactive mode

open System
open System.IO

let sizeOfFolder folder =
    
    let filesInFolder : string [] =
        Directory.GetFiles(
            folder, "*.*",
            SearchOption.AllDirectories)

    let fileInfos : FileInfo [] =
        Array.map
            (fun (file : string) -> new FileInfo(file))
            filesInFolder

    let fileSizes : int64 [] =
        Array.map
            (fun (info : FileInfo) -> info.Length)
            fileInfos

    let totalSize = Array.sum fileSizes

    // dependency:
    // totalSize <- fileSizes <- fileInfos <- filesInFolder <- folder
    totalSize;;


sizeOfFolder ".";;
// size of folder

// here comes "pipe-forward operator"
// actually we've once met with it

[ 1 .. 3 ] |> List.iter (printfn "%d");;

// redefine sizeOfFolder

let sizeOfFolderPiped folder =
    let getFiles folder1 =
        Directory.GetFiles(folder1, "*.*", SearchOption.AllDirectories)

    let totalSize =
        folder
        |> getFiles
        |> Array.map (fun file -> new FileInfo(file))
        |> Array.map (fun info -> info.Length)
        |> Array.sum

    totalSize;;

sizeOfFolderPiped ".";;

// might cause error ...
// but I believe we've had enough info to infer type, anyway
// List.iter
//    (fun s -> printfn "s has length %d" s.Length)
//    ["Pipe"; "Forward"];;

["Pipe"; "Forward"] |> List.iter (fun s -> printfn "s has length %d" s.Length);;
// works

let sizeOfFolderComposed1 folder =
    let getFiles folder1 =
        Directory.GetFiles(folder1, "*.*", SearchOption.AllDirectories)

    (  getFiles
    >> Array.map (fun file -> new FileInfo(file))
    >> Array.map (fun info -> info.Length)
    >> Array.sum) folder;;

sizeOfFolderComposed1 ".";;

// or we can wipe out parameters
// I like "sizeOfFolderComposed1", 
// why should the first function aligned differently, anyway...
let sizeOfFolderComposed2 =
    let getFiles folder1 =
        Directory.GetFiles(folder1, "*.*", SearchOption.AllDirectories)

    getFiles
    >> Array.map (fun file -> new FileInfo(file))
    >> Array.map (fun info -> info.Length)
    >> Array.sum;;

sizeOfFolderComposed2 ".";;

// another example for function composition
let square x = x * x
let toString (x:int) = x.ToString()
let strLen (x:string) = x.Length
let lenOfSquare = square >> toString >> strLen;;

lenOfSquare 1024;;
// 7

List.iter (printfn "%d") [1..3];;
List.iter (printfn "%d") <| [1..3];;

printfn "The result of sprintf is %s" (sprintf "(%d,%d)" 1 2);;
// one like "$" in Haskell?
printfn "The result of sprintf is %s" <| sprintf "(%d,%d)" 1 2;;

// backward composition
let negate x = -x;;
(square >> negate) -2;;
// -4
(square << negate) -2;;
// 4

// filtering lists
[ [1]; []; [4;5;6]; [3;4]; []; []; []; [9] ]
    |> List.filter (not << List.isEmpty);;
// select only those lists that are not empty

#quit;;
