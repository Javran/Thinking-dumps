let square x = x * x

let imperativeSum numbers =
    let mutable total = 0
    for i in numbers do 
        let x = square i
        total <- total + x
    total

let functionalSum1 numbers =
    Seq.sum (Seq.map square numbers)

// seems '|>'s are used to compose functions
let functionalSum2 numbers =
    numbers
    |> Seq.map square
    |> Seq.sum


let intList = [1 .. 10]

imperativeSum intList;;
functionalSum1 intList;;
functionalSum2 intList;;

#quit;;
