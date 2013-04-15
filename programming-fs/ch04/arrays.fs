// note this file only works in interactive mode

// arrays are:
// * mutable
// * quick random access
// * continguous blocks of memory
// * array comprehension is available (similar to its list counterpart)

let perfectSquares = [| for i in 1..7 -> i * i |];;
let perfectSquares2 = [| 1; 4; 9; 16; 25 |];;

// indexing an array

// retrieve an element from array: use .[]
printfn "The first three perfect square are %d, %d and %d"
    perfectSquares.[0]
    perfectSquares.[1]
    perfectSquares.[2];;

printfn "Print first 3 perfect square:"

// arrays are "Seq"-compatible as well
perfectSquares
    |> Seq.take 3
    |> Seq.iter (printfn "%d");;

open System

let rot13Encrypt (letter: char) =
   match letter with
   // note: this function works only for uppercase letters
   | _ when Char.IsLetter(letter) ->
        (int letter)
        |> fun letterIdx -> letterIdx - (int 'A')
        |> fun letterIdx -> (letterIdx + 13) % 26
        |> fun letterIdx -> letterIdx + (int 'A')
        |> char
   | _ ->
        letter;;

let encryptTest (text : char[]) =
    for idx = 0 to text.Length - 1 do
        let letter = text.[idx]
        // note here we use "<-" to assign new value to a variable
        text.[idx] <- rot13Encrypt letter

let text =
    Array.ofSeq "THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG"

printfn "Original: %s" <| new String(text)
encryptTest text
printfn "Encrypted: %s" <| new String(text)
// for ROT13, to redo encryption on the encrypted text is to decrypt
encryptTest text
printfn "Decrypted: %s" <| new String(text)

// use .Length to access the last element
let alphabet = [| 'a' .. 'z' |];;
printfn "first: %c, last: %c"
    alphabet.[0]
    alphabet.[alphabet.Length - 1];;

// the line above would run into IndexOutOfRangeException
// alphabet.[1000];;

// array slices
// array.[lowerBound..upperBound]
// * default value of lowerBound is 0
// * default value of upperBound is len-1

open System
let daysOfWeek = Enum.GetNames( typeof<DayOfWeek> )

daysOfWeek.[2..4];;
// Tues, Wed, Thurs

daysOfWeek.[4..];;
// Thurs, Fri, Sat

daysOfWeek.[..2];;
// Sun, Mon, Tues

// we can even give no bounds at all by '*', which means to copy the array
let daysOfWeekClone = daysOfWeek.[*];;

Seq.iter (fun ind -> ignore <| daysOfWeekClone.[ind] <- sprintf "%d" ind) [for x = 0 to daysOfWeekClone.Length - 1 do yield x];;

daysOfWeek;;
daysOfWeekClone;;
// changed to numbers but the original one is not altered 

// creating arrays
let divisions = 4.0
let twoPi = 2.0 * Math.PI;;

Array.init
    (int divisions) // initialize 4 elements
    (fun i -> float i * twoPi / divisions);; // for each element, initialize it by its index

let emptyIntArray : int [] = Array.zeroCreate 3;; // initialized with 0(primitive type)
let emptyStringArray : string [] = Array.zeroCreate 3;; // initialized with null

// pattern matching
let describeArray =
    function
    | null ->
        "The array is null"
    | [| |] ->
        "The array is empty"
    | [| x |] ->
        sprintf "The array has one element, %A" x
    | [| x; y |] ->
        sprintf "The array has two elements, %A and %A" x y
    | a ->
        sprintf "The array had %d elements, %A" a.Length a;;

describeArray null;;
describeArray [| |];;
describeArray [| 1; 2; 3 |];; 
describeArray [| (1,2,3) |];;

// array equality:
// * same rank(dimension)
// * same lenth
// * same elements

[| 1 .. 2 .. 10 |] = [| 1; 2; 3; 4; 5 |];;
// false

[| 1 .. 2 .. 10 |] = Array.map (fun x -> x*2-1) [| 1; 2; 3; 4; 5 |];;
// true

// array module functions

// Array.init 
Array.init 10 (fun ind -> ind);;
// [|0;1;2...;9|]

// Array.zeroCreate
(Array.zeroCreate 10 : int []);;
// [|0;0;0...;0|]

let isGreaterThanTen x = x > 10;;

// Array.partition: break an array into 2 arraries according to if the element meets a condition
Array.init 10 (fun x -> 2*x+1) |> Array.partition isGreaterThanTen;;

// Array.tryFind
let rec isPowerOfTwo =
    function
    | 2 ->
        true
    | x when x % 2 = 1 ->
        false
    | x ->
        isPowerOfTwo (x/2);;

let testCase1 = [| 1; 7; 13; 64; 32 |]

Array.tryFind isPowerOfTwo testCase1;;
// Some 64

Array.tryFindIndex isPowerOfTwo testCase1;;
// Some 3
        
// aggregate operators
let vowels = Array.ofSeq "aeiou";;

Array.iteri (fun idx chr -> printfn "vowel.[%d] = %c" idx chr) vowels;;

// multidimentional arrays
// * rectangle arrays, including Array2D and Array3D
// * jagged arrays 
//     (I think there is no difference between this kind of arrays and regular arrays)

let identityMatrix1 = Array2D.zeroCreate 3 3
identityMatrix1.[0,0] <- 1.0
identityMatrix1.[1,1] <- 1.0
identityMatrix1.[2,2] <- 1.0

let identityMatrix2 =
    let constructIdentity x y =
        if x = y
            then 1.0
            else 0.0

    Array2D.init 3 3 constructIdentity;;
    
identityMatrix1 = identityMatrix2;;
// true

// supports slicing
identityMatrix2.[*,1..2];;

Array2D.init 2 4 (fun x y-> sprintf "(%d,%d)" x y);;
Array3D.init 2 3 4 (fun x y z -> 100*x + 10*y + z);;

let jaggedArray1 : int[][] = Array.zeroCreate 3
jaggedArray1.[0] <- Array.init 1 (fun x -> x)
jaggedArray1.[1] <- Array.init 2 (fun x -> x)
jaggedArray1.[2] <- Array.init 3 (fun x -> x);;

let jaggedArray2 : int[][] = 
    Array.init 3 (fun x -> Array.init (x+1) (fun x -> x) );;

jaggedArray1 = jaggedArray2;;
// true

#quit;;
