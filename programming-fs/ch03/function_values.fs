// note this file only works in interactive mode

let negate x = -x;;
List.map negate [0..10..100];;

// here comes our old friend: lambda expression!

List.map (fun x -> x + 3) [1..10];;
// [4 .. 13]

[1..10]
    |> List.map (fun x -> x + 3)
    |> (fun l -> l = [4..13]);;

// partial function application

open System.IO

let appendFile (fileName : string) (text : string) =
    use file = new StreamWriter( fileName, true )
    file.WriteLine( text )
    file.Close();;

appendFile @"Log.txt" "Processing Event X ...";;

// now `appendLogFile` is a partial applied version of function `appendFile`
let appendLogFile = appendFile @"Log.txt"

appendLogFile "Processing Event Y ...";;

List.iter (fun x -> appendLogFile (sprintf "Processing Data %c ..." x)) ['A' .. 'Z'];;

List.iter (printfn "%d") [1..10];;

// functions returning functions

// you can see that actually we can define a function that needs 2 arguments
//     and use it as an partial function
let generatePowerOfFunc1 baseValue exponent = baseValue ** exponent;;
let generatePowerOfFunc2 baseValue =
    (fun exponent -> baseValue ** exponent);;

let powerOfTwo1 = generatePowerOfFunc1 2.0;;
let powerOfTwo2 = generatePowerOfFunc2 2.0;;

powerOfTwo1 8.0;;
powerOfTwo2 8.0;;
// 256.0

let powerOfThree = generatePowerOfFunc1 3.0;;
powerOfThree 2.0;;
// 9.0

#quit;;
