// note this file only works in interactive mode

// each discriminated union case is a distinctly different class ... might be heavyweight
// an enumeration is just a wrapper over integral type

// creating enumeration
type ChessPiece =
    | Empty     = 0
    | Pawn      = 1
    | Knight    = 3
    | Bishop    = 4
    | Rook      = 5
    | Queen     = 8
    | King      = 10

let createChessBoard () =
    let board = Array2D.init 8 8 (fun _ _ -> ChessPiece.Empty)

    // place pawns
    for i = 0 to 7 do
        board.[1,i] <- ChessPiece.Pawn
        board.[6,i] <- enum<ChessPiece> (-1 * int ChessPiece.Pawn)

    // place black & white pieces in order
    [| ChessPiece.Rook; ChessPiece.Knight; ChessPiece.Bishop; ChessPiece.Queen;
       ChessPiece.King; ChessPiece.Bishop; ChessPiece.Knight; ChessPiece.Rook |]
        |> Array.iteri 
            (fun idx piece ->
                board.[0,idx] <- piece
                board.[7,idx] <- enum<ChessPiece> (-1* int piece))
    board

createChessBoard();;

// we can use pattern matching on enumeration as well ...
let isPawn =
    function
    | ChessPiece.Pawn -> true
    | _ -> false

isPawn ChessPiece.Pawn;;
// true
isPawn ChessPiece.Bishop;;
// false

// conversion
let invalidPiece = enum<ChessPiece>(42);;
let materialValueOfQueen = int ChessPiece.Queen;;
// 8

// check the existence of an enum value:
System.Enum.IsDefined(typeof<ChessPiece>, int ChessPiece.Rook);;
// true
System.Enum.IsDefined(typeof<ChessPiece>, 12345);;
// false

#quit;;
