// note this file only works in interactive mode

let rec (!) x =
    if x <= 1
        then 1
        else x * !(x - 1);;

List.map (!) [ 1 .. 10 ];;
// 1, 2, 6, 24 ..

!5;;
// 120

open System.Text.RegularExpressions;;

let (===) str (regex : string) =
    Regex.Match(str, regex).Success;;

// infix by default:
"The quick brown fox" === "The (.*) fox";;
// true

// what it actually is
(===) "The quick brown fox" "The (.*) Fox";;
// false

// pass it to other function (actually there's no difference)
List.fold (+) 0 [1..10];;
// 55

// "(*" might be wrongly recognized (by some editor)as the begin of a block comment
List.fold ( * ) 1  [1..10] = ! 10;;
// true

// again (-) is a function
let minus = (-)
List.fold minus 56 [1..10];;
// 1

#quit;;
