// note this file only works in interactive mode

open System.IO

// single case active patterns
let determineFileType (filePath:string) =
    let (| FileExtension |) filePath = Path.GetExtension(filePath)
    match filePath with
    // without active patterns
    | filePath when Path.GetExtension(filePath) = ".txt" ->
        printfn "It is a text file."

    // converting the data using an active pattern
    | FileExtension ".jpg"
    | FileExtension ".png"
    | FileExtension ".gif" ->
        printfn "It is an image file."

    // binding a new value
    | FileExtension ext ->
        printfn "Unknown file extension [%s]" ext

[ "aaa.txt"; "aaa.jpg"; "aaa.png"; "aaa.dat" ]
    |> Seq.iter determineFileType



#quit;;
