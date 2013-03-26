// note this file only works in interactive mode

let rec factorial x =
    if x <= 1
        then 1
        else x * factorial (x-1);;

factorial 5;;
//120

let rec forLoop body times =
    if times <= 0
        then ()
        else
            body()
            forLoop body (times - 1)

let rec whileLoop predicate body =
    if predicate()
        then
            body()
            whileLoop predicate body
        else
            ();;

forLoop (fun() -> printfn "Looping ..." ) 3;;

open System

whileLoop
    (fun() -> DateTime.Now.Second % 2 <> 0)
    (fun() -> printfn "foo ...");;
// keep printing "foo ..." for (0 - 2 sec)

// for mutually recursive functions, we need keyword 'and'

let rec isOdd x =
    if x = 0
        then false
    elif x = 1
        then true
    else
        isEven (x-1)
and isEven x =
    if x = 0
        then true
    elif x = 1
        then false
    else
        isOdd (x-1)

isOdd 214;;
// false
isEven 214;;
// true
        

#quit;;
