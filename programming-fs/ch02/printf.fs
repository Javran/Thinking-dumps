// note this file only works in interactive mode

printf "Nice, "
printfn "Boat!";;

let mountain = "K2"
let height = 8611
let units = 'm';;

printfn "%s is %d%c high" mountain height units;;

// type inference
let inferParams x y z =
    printfn "x = %f, y = %s, z = %b" x y z;;
// the type of 'inferParams' should be:
// float -> string -> bool -> unit()

// use 'sprintf' for formatting strings
let location = "Boat";;
sprintf "Nice, %s" location;;

#quit;;
