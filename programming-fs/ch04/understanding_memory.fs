// note this file only works in interactive mode

printfn "%A" <| Unchecked.defaultof<int>;;
// 0
printfn "%A" <| typeof<int>;;
// System.Int32
printfn "%A" <| sizeof<int>;;
// 4

let isNull =
    function
       | null ->
            true
       | _ ->
            false;;

isNull "a string";;
// false
isNull (null : string);;
// true

type Thing =
    | Planet
    | Animal
    | Mineral

// line below would fail
//(null : Thing);;

// aliasing
let x = [| 0 |]
let y = x;;

x.[0] <- 3;;

printfn "%A %A" x y;;
// all changes to [|3|]

y.[0] <- 1;;
printfn "%A %A" x y;;
// all changes to [|1|]

#quit;;
