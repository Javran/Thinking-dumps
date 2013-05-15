// note this file only works in interactive mode

open System.IO

let asyncProcessFile 
        (srcFilePath : string)
        (dstFilePath : string) 
        (processBytes : byte [] -> byte []) =
    async {
        printfn "Processing file: [%s]" (Path.GetFileName(srcFilePath))

        let fileStream = new FileStream(srcFilePath, FileMode.Open)
        let bytesToRead = int fileStream.Length

        let! data = fileStream.AsyncRead(bytesToRead)
        // we need to close file as soon as we finish using it
        fileStream.Close()

        printfn "Opened [%s], read [%d] bytes" (Path.GetFileName(srcFilePath)) data.Length

        let data' = processBytes data

        use resultFile = new FileStream(dstFilePath, FileMode.Create)
        do! resultFile.AsyncWrite(data',0, data'.Length)
        // we need to close file as soon as we finish using it
        resultFile.Close()

        printfn "Finished processing file [%s]" <| Path.GetFileName(srcFilePath)
    } |> Async.RunSynchronously;; // seems when we make it run synchronously, the output will be as expected

let asyncIOTest () =

    // create some file for testing
    let testIn = "infile.tmp"
    let testOut = "outfile.tmp"

    // data processing is ignored here, so what we just do is simply making a copy of the original file
    let processBytes b = b

    File.WriteAllLines(testIn, seq {for i = 1 to 1000 do yield sprintf "%d" i} )

    asyncProcessFile testIn testOut processBytes

    File.Delete(testIn)
    File.Delete(testOut)

// uncomment to see the example:
//asyncIOTest();;

open System.Net

let getHtml (url : string) =
    async {
        let req = WebRequest.Create(url)
        let! rsp = req.AsyncGetResponse()

        use stream = rsp.GetResponseStream()
        use reader = new StreamReader(stream)
        return reader.ReadToEndAsync().Result
        // I guess here we call "Result" to wait for the result
    }

let testDownload () =
    let content = 
        getHtml "http://en.wikipedia.org/"
        |> Async.RunSynchronously
    printfn "Length of en.wikipedia.org: %d" content.Length

    let webPages : string [] =
        [ "http://www.google.com"
        ; "http://www.bing.com"
        ; "http://www.yahoo.com" ]
        |> List.map getHtml
        |> Async.Parallel
        |> Async.RunSynchronously
        |> Array.map (fun x -> sprintf "Length: %d" x.Length)
        
    webPages;;
        
// uncomment to see the example
//testDownload();;

// test some computations that might fail
let testComputationMightFail () =
    let asyncTasks =
        [ async { raise <| new System.Exception("Error") } 
        ; async { () } ]
    asyncTasks
    |> Async.Parallel
    |> Async.Catch
    |> Async.RunSynchronously;;
    
// uncomment to see the example
// testComputationMightFail();;

open System
open System.Threading

let cancelableTask =
    async {
        printfn "Waiting 1 sec ..."
        for i = 1 to 10 do
            printfn "%d ..." i
            do! Async.Sleep(100)
        printfn "Finished"
    };;

let cancelHandler (ex : OperationCanceledException) =
    printfn "The task has been canceled."

let testCalcelable () =
    Async.TryCancelled(cancelableTask, cancelHandler)
    |> Async.Start
    Thread.Sleep(300)
    Async.CancelDefaultToken();;

// uncomment to see the example
// testCalcelable ();;

let testAsyncStartWithContinuations () =
    let asyncTask = 
        async {
            for i = 1 to 10 do
                printfn "Working..."
                do! Async.Sleep(100) 
        }

    Async.StartWithContinuations(
        asyncTask,
        (fun result ->
            printfn "Task completed with result: %A" result),
        (fun ex ->
            printfn "Task threw an exception: %A" ex.Message),
        (fun oce ->
            printfn "Task was cancelled: %A" oce.Message))

    Thread.Sleep(200)
    Async.CancelDefaultToken()

// uncomment to see the example
// testAsyncStartWithContinuations();;

#quit;;
