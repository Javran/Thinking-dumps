open System


[<EntryPoint>]
let main (args: string []) =
    let numbers = [1..10]
    let square x = x * x

    let squaredNumbers = List.map square numbers

    printfn "SquaredNumbers = %A" squaredNumbers

    printfn "(press any key to continue)"

    Console.ReadKey( true ) |> ignore

    0
