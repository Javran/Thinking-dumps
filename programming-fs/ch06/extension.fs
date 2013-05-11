// note this file only works in interactive mode

// extend the System.Int32 type
type System.Int32 with
    member this.ToHexString() = sprintf "0x%x" this;;

(123456).ToHexString();;

#load "fscollectionextensions.fs"

// the following 2 lines will fail because we have not yet open the namespace
//     where our extension methods are located
// List.skip 5 [1..10];;
// Seq.rev [1..10];;

open FSCollectionExtensions
List.skip 5 [1..10];;
Seq.rev [1..10];;

#quit;;
