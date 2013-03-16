// note this file only works in interactive mode

let square x = x * x;;

square 4;;
// 16

let addOne1 x = x + 1;;
let addOne2 = (+) 1;;

addOne1 10;;
addOne2 10;;
// 11

let add1 x y = x + y;;
let add2 = (+);;

add1 10 20;;
add2 10 20;;
// 30

let mult x y = x * y
// only works on int
mult 10 20;;

// not working:
// mult 10.0 20.0;;

// infer type
let multi x y = x * y
let result = multi 1.23 4.56;;

let add3 (x: float) y = x + y;;

add3 1.0 2.0;;

// identity function
let ident x = x;;

ident "a string";;
ident 1234L;;
ident 34uy;;

// force generic
let ident2 (x : 'a) = x;;

// scope
let moduleValue = 10
let functionA x = x + moduleValue;;

let functionB x =
    let functionValue = 20
    x + functionValue

// not working:
//functionValue;;

// nested functions
let moduleValue2 = 1
let f fParam =
    let g gParam = fParam + gParam + moduleValue2

    let a = g 1
    let b = g 2
    a + b;;

f 7;;
// g 1 -> 7+1+1
// g 2 -> 7+2+1
// a + b -> 19

// here the code in the book looks awful, skipping it.
let bytesToGB x =
    let x_2 = x   / 1024I //  B -> KB
    let x_3 = x_2 / 1024I // KB -> MB
    let x_4 = x_3 / 1024I // MB -> GB
    x_4;;

let hardDriveSize = bytesToGB 268435456000I;;

// control flow
let printGreeting shouldGreet greeting =
    if shouldGreet then
        printfn "%s" greeting;;

printGreeting true "Nice!";;
printGreeting false "Nice Boat!";;

let isEven x =
    let result =
        if x % 2 = 0 
            then "Yes it is"
            else "No it is not"
    result;;

isEven 5;;
isEven 6;;

let isWeekday day =
    if   day = "Moday"      then true
    elif day = "Tuesday"    then true
    elif day = "Wednesday"  then true
    elif day = "Thursday"   then true
    elif day = "Friday"     then true
    else false;;

isWeekday "Sunday";;
// false
isWeekday "Thursday";;
// true

#quit;;
