// note this file only works in interactive mode

// count up from zero

let mutable i = 0

// while loop
while i < 5 do
    i <- i + 1
    printfn "i = %d" i;;

// for loop
for i = 1 to 5 do
    printfn "i = %d" i;;

for i = 5 downto 1 do
    printfn "i = %d" i;;

for i in [1..5] do
    printfn "i = %d" i;;

for i in [1..2..10] do
    printfn "i = %d" i;;
// 1,3,5,7,9

// for - pattern matching

type Pet =
    | Cat of string * int // Name, Lives
    | Dog of string

let famousPets =
    [ Dog("Lassie")
    ; Cat("Felix", 9)
    ; Dog("Rin Tin Tin")
    ];;

// to suppress the warning here, 
// please refer to:
// http://stackoverflow.com/questions/5628151/using-incomplete-pattern-matching-as-filter
for pet in famousPets do
    match pet with
    | Dog(name) ->
        printfn "%s was a famous dog." name
    | _ ->
        ();;

#quit;;
