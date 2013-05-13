// note this file only works in interactive mode

// APM: asynchronous programming model
// call BeginOperation at first and call EndOperation to retrieve the result

open System
open System.IO

let processFileAsync (srcFilePath : string) (dstFilePath : string) (processBytes : byte [] -> byte []) =

    let asyncWriteCallback = new AsyncCallback( fun (iar: IAsyncResult) ->
        let writeStream = iar.AsyncState :?> FileStream

        let bytesWritten = writeStream.EndWrite(iar)
        writeStream.Close()

        printfn
            "Finished processing file [%s]"
            (Path.GetFileName(writeStream.Name)))
    
    
    // will be called when the async read completes
    let asyncReadCallback = new AsyncCallback( fun (iar: IAsyncResult) ->
        // unpacking state passed
        let readStream, data = iar.AsyncState :?> (FileStream * byte [])

        let bytesRead = readStream.EndRead(iar)
        readStream.Close()

        printfn 
            "Processing file [%s], read [%d] bytes"
            (Path.GetFileName(readStream.Name))
            bytesRead

        let updatedBytes = processBytes data

        let resultFile = new FileStream(dstFilePath, FileMode.Create)

        let _ = resultFile.BeginWrite(updatedBytes, 0, updatedBytes.Length, asyncWriteCallback, resultFile)

        ())

    let fileStream = new FileStream(srcFilePath, FileMode.Open, FileAccess.Read, FileShare.Read, 2048, FileOptions.Asynchronous)
    let fileLength = int fileStream.Length
    let buffer = Array.zeroCreate fileLength

    printfn "Processing file [%s]" (Path.GetFileName(srcFilePath))

    // wrapping state to get ready for passing
    let state = (fileStream, buffer)

    let _ = fileStream.BeginRead(buffer, 0, buffer.Length, asyncReadCallback, state)

    ();;

// create some file for testing
let testIn = "infile.tmp"
let testOut = "outfile.tmp"

// data processing is ignored here, so what we just do is simply making a copy of the original file
let processBytes b = b

File.WriteAllLines(testIn, seq {for i = 1 to 1000 do yield sprintf "%d" i} );;

processFileAsync testIn testOut processBytes;;

File.Delete(testIn);;
File.Delete(testOut);;

#quit;;
