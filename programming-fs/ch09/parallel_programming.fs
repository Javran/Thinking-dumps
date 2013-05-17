// note this file only works in interactive mode

open System
open System.Threading.Tasks

let matrixMultiply (a : float[,]) (b : float[,]) =
    let aRow, aCol = Array2D.length1 a, Array2D.length2 a
    let bRow, bCol = Array2D.length1 b, Array2D.length2 b

    if aCol <> bRow
        then failwith "Array dimension mismatch"

    let c = Array2D.create aCol aRow 0.0
    let cRow, cCol = aCol, bRow

    let rowTask rowIdx =
        for colIdx = 0 to cCol - 1 do
            for x = 0 to aRow - 1 do
                c.[colIdx, rowIdx] <-
                    c.[colIdx, rowIdx] + a.[x,colIdx] * b.[rowIdx, x]

    let _ = Parallel.For(0, cRow, new Action<int>(rowTask >> ignore))

    c;;

let testMatMul () =
    let x = 
        array2D
            [| [| 1.0; 0.0 |]
            ;  [| 5.0; 1.0 |] |]
    let y =
        array2D
            [| [| 1.0; 2.0 |]
            ;  [| 7.0; 8.0 |] |]
    matrixMultiply y x;;

// uncomment to see the example
// testMatMul();;

// skip Array.Parallel Module, nothing new

#quit;;
