// note this file only works in interactive mode

// use 'let' can create a value

let x = 1;;

let answerToEverything = 42UL;;
// primitive type: uint64

let pi = 3.1415M;;
// primitive type: decimal

let avogadro = 6.022e23;;
// primitive type: float

let hex, oct, bin = 0xFCAF, 0o7771L, 0b00101010y;;
// base 16, 8, 2

(hex, oct, bin)

// please refer to IEEE32/IEEE64
// LF for float
0x401E000000000000LF;;

// lf for float32
0x00000000lf;;

// might overflow
32767s + 1s;;
-32768s + -1s;;

// show me the bigint!
open System.Numerics

let megabyte = 1024I * 1024I;;
let zettabyte = megabyte * 1024I * 1024I * 1024I * 1024I * 1024I;;

// bitwise
0b1111 &&& 0b0011;; // 0b0011( 3)
0b1010 ||| 0b0111;; // 0b1111(15)
0b0011 ^^^ 0b0101;; // 0b0110( 6)

// left/right shift
2 <<< 10 ;; // 2048
2048 >>> 10 ;; // 2
0b1100 >>> 2 ;; // 3

// characters

let vowels = ['a'; 'e'; 'i'; 'o'; 'u'];;

// string formatting
printfn "Hex u0061 = '%c'" '\u0061';;

// conversion

int 'C';;
// 67

'C'B;;
// byte: 67uy ('uy' for unsigned byte)

let password = "abracadabra";;
let multiline = "This string
takes up
multiple lines";;

multiline.[0];;
// T
multiline.[1];;
multiline.[2];;
multiline.[3];;

// triple quotes

let xmlFragment = """<Ship Name="Prometheus"></foo>""";;

let longString = "abc-\
                    def-\
                ghi";;
// "abc-def-ghi"

let normalString = "Normal.\n.\n.\t.\t.String";;
let verbatimString = @"Verbatim.\n.\n.\t.\t.String";;

// string to byte array
let hello = "Hello"B;;
// [|72uy; 101uy; 108uy; 108uy; 111uy|]

// Booleans:
// operators are: || && not

let printTruthTable f =
    printfn ""
    printfn "       |  true | false |"
    printfn "       +-------+-------+"
    printfn " true  | %5b | %5b |" (f true true) (f true false)
    printfn " false | %5b | %5b |" (f false true) (f false false)
    printfn "       +-------+-------+"
    printfn ""
    ();;

// print truth table for &&
printTruthTable (&&);;
printTruthTable (||);;

#quit;;
