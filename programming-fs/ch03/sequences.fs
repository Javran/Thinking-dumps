// note this file only works in interactive mode

let seqOfNumbers = seq { 1 .. 5 };;
// they don't get evaluated when initialized

seqOfNumbers |> Seq.iter (printfn "%d");;
// get evaluated on demand

// all positive integers
let allPositiveIntSeq =
    seq {
        for i in 1 .. System.Int32.MaxValue do
            yield i } ;;
let first20PositiveIntSeq = Seq.take 20 allPositiveIntSeq;;
// we can see that take is still lazy

Seq.iter (printfn "%d") first20PositiveIntSeq;;

// sequence expressions

let alphabet = seq { for c in 'A' .. 'Z' -> c };;

// print 'C' to 'X' only
alphabet 
    |> Seq.take 24 // only 'A' .. 'X' are kept
    |> Seq.skip 2 // remove 'A' .. 'B'
    |> Seq.iter (printf "%c") // print all
// CDE....VWX

printfn ""

let noisyAlphabet =
    seq {
        for c in 'A' .. 'Z' do
            printfn "Yielding %c ..." c
            yield c
    };;

let fifthLetter = Seq.nth 4 noisyAlphabet;;
// first 5 elements are all evaluated to get the last one as result

Seq.nth 0 noisyAlphabet;;

noisyAlphabet
    |> Seq.take 2
    |> Seq.iter (printf "%c")

printf "";;
// here only 'A' and 'B' are yielded

open System.IO

let rec allFilesUnder basePath =
    seq {
        // yield all files in the base folder
        yield! Directory.GetFiles( basePath )

        // yield all files in its sub folders
        for subdir in Directory.GetDirectories( basePath ) do
            yield! allFilesUnder subdir
    };;

// get all files under this dir
Seq.iter (printfn "%s") <| allFilesUnder ".";;

// get all files under the parent dir of the current dir
Seq.iter (printfn "%s") <| allFilesUnder "..";;

// seq module functions

// all can be found at:
// http://msdn.microsoft.com/en-us/library/ee353635.aspx 

open System

let randomSeq =
    seq { 
        let rng = new Random()
        while true do
            yield rng.Next()
    };;

// print some random numbers
randomSeq
    |> Seq.take 5
    |> Seq.iter (printfn "%A")

// Seq.unfold: 2 args
// arg1: 'a -> ('b * 'a) option
//     accepts a value 'a
//     produce 'b that will be put in the seq,
//     the next 'a is the second item of the resulting tuple
//     return None if no more things can be produced from this seq
// arg2: 'a
//     the seed

let nextFibUnder100 (fibMinus1, fibMinus2) =
    let fib = fibMinus1 + fibMinus2
    
    if fib > 100 then
        None
    else
        Some( fib, (fib, fibMinus1) );;

Seq.toList <| Seq.unfold nextFibUnder100 (0,1);;
// toList forces the seq to be eval-ed
// [1; 1; 2; 3; .. 89 ]

// explore other functions:

let testSeq = seq { for i in 1 .. 100 do yield i };;

Seq.length testSeq;;
// 100

Seq.exists ((=) 10) testSeq;;
// true

Seq.exists ((=) 0) testSeq;;
// false

Seq.tryFind (fun (x) -> x > 100) testSeq;; 
// None

Seq.length <| Seq.filter (fun (x) -> x % 2 = 0) testSeq;;
// 50

seq {
    for i in 1 .. 10 do
        yield testSeq }

    |> Seq.concat
    |> Seq.length
    |> printfn "%d";;
// seq len = 10 * 100 = 1000

seq {
    for i in 1 .. 10 do
        yield! testSeq}
    |> Seq.length
    |> printfn "%d";;
// I think "yield!" actually provides another way of flattening a list
// this also produce "1000" as expected

#quit;;
