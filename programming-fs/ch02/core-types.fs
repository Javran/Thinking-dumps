// note this file only works in interactive mode

// unit
let x = ();;

// we can ignore results using function "ignore"

let square x = x * x;;
square 4;;

ignore( square 4 );;
// will now result in "unit"

// tuple
let dinner = ("green eggs", "ham");;
// note here the tuple is "string * string"

let zeros = (0, 0L, 0I, 0.0);;
// the type is: (int, int64, bigint, float)

let nested = (1, (2.0, 3M), (4L, "5", '6'));;

// we can use 'fst' and 'snd' to deal with tuples
//     as what we've always got used to in Haskell :)

let nameTuple = ("John", "Smith");;

fst nameTuple;;
// John
snd nameTuple;;
// Smith

// or we can extract values using pattern matching
let snacks = ("Soda", "Cookies", "Candy");;

let x, y, z = snacks;;

y, z;;

// tuples are first-order structures
//     i.e.: be used as arguments/return values/etc.
let add x y = x + y;;
// tupleAdd is just like uncurried add
let tupledAdd (x,y) = add x y;;

add 3 7;;
tupledAdd (3,7);;
// 10

#quit;;
