open System

(*
    block comments
*)

// to run 'main' with proper arguments, you can type in:
// main [| "Nice"; "boat" |];;
// in interactive mode

[<EntryPoint>]
let main (args: string[]) =
    if args.Length <> 2 then
        failwith "Error: Expected arguments <greeting> and <thing>"

    let greeting, thing = args.[0], args.[1]
    let timeOfDay = DateTime.Now.ToString("hh:mm tt")

    printfn "%s, %s at %s" greeting thing timeOfDay

    // variable name is case sensitive
    let number = 1
    let Number = 2
    printfn "number is %d, Number is %d" number Number

    // Program exit code
    0
