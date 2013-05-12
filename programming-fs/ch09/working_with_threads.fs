// note this file only works in interactive mode

// spawning threads

open System
open System.Threading

let threadBody() =
    for i in 1 .. 5 do
        Thread.Sleep(100)

        printfn "[Thread %d] %d ..."
            Thread.CurrentThread.ManagedThreadId
            i

let spawnThread() =
    let thread = new Thread(threadBody)
    thread.Start()

let testSpawnThread () =
    spawnThread()
    spawnThread();;


// uncomment next line to see the example
//testSpawnThread();;

// the .Net thread pool

open System.Threading

let testThreadPool () =
    ThreadPool.QueueUserWorkItem(fun _ ->
            for i = 1 to 5 do
                printfn "[%d]" i) |> ignore

    let printNumbers (max : obj) =
        for i = 1 to (max :?> int) do
            printfn "<%d>" i
            
    ThreadPool.QueueUserWorkItem(new WaitCallback(printNumbers), box 5) |> ignore ;;

// uncomment next line to see the example
//testThreadPool();;

// sharing data

// rare condition: use lock to deal with data race

let lockedSumArray(arr : int[]) =
    let total = ref 0

    let countTask a b onDone = fun _ ->
        printfn "Starting..."
        for i = a to b do
            lock (total) (fun () -> total := arr.[i] + !total)
        onDone()
        printfn "Done."

    let thread1Finished = ref false
    let thread2Finished = ref false

    ThreadPool.QueueUserWorkItem(
        new WaitCallback(
            countTask 0 (arr.Length/2 - 1) (fun () -> thread1Finished := true))) |> ignore

    ThreadPool.QueueUserWorkItem(
        new WaitCallback(
            countTask (arr.Length/2) (arr.Length-1) (fun () -> thread2Finished := true))) |> ignore

    // the book has made a mistake here:
    // only cond to break the loop is when all of threadXFinished is set to true
    while not (!thread1Finished && !thread2Finished) do
        Thread.Sleep(10)

    !total;;
           
let testResourceRace () =
    let bigArr = Array.create 1000000 1
    lockedSumArray bigArr

testResourceRace();;

#quit;;
