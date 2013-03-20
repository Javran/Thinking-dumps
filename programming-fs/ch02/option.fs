// note this file only works in interactive mode

open System

// I guess 'option' is the equivalent of 'Maybe' in Haskell
let isInteger str =
    let successful, result = Int32.TryParse str
    if successful
        then Some(result)
        else None;;

isInteger "This is not an int" ;;
// None
isInteger "400";;
// Some 400


// actually I don't think this is a good example of how to use Option
//     since we can return an empty list to indicate 
//     that we cannot find any negative number in a given list
// Just like what Maybe would be used,
//     Option should be used on some kind of operations that might fail
//     e.g. convert string to num / do File IO / download files ...

let isLessThanZero x = x < 0
let containsNegativeNumbers intList =
    let filterdList = List.filter isLessThanZero intList
    if List.length filterdList > 0
        then Some filterdList
        else None;;

let negativeNumbers = containsNegativeNumbers [6; 20; -8; 45; -5];;

// get back the list
Option.get negativeNumbers;;
// [-8; -5]

#quit;;
