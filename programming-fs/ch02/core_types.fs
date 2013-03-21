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

let vowels = ['a'; 'e'; 'i'; 'o'; 'u'];;
let emptyList = [];;

// cons operator
let sometimes = 'y' :: vowels;;

// append operator
let odds = [1; 3; 5; 7; 9]
let evens = [2; 4; 6; 8; 10]

odds @ evens;;

// list ranges

let x = [1 .. 10];;
let tens = [0 .. 10 .. 50];;
let countDown = [ 5L .. -1L .. 0L ];;
let countDownBy x = [ 100 .. (-x) .. 0 ];;

countDownBy 7;;
// [100; 93; 86; .. ]

// list comprehension
let numbersNear x =
    [
        yield x - 1
        yield x
        yield x + 1
    ];;

numbersNear 3;;
// [2; 3; 4];;

// more complex list comprehension
let x =
    [
        let negate x = -x
        for i in 1..10 do
            if i % 2 = 0 then
                yield negate i
            else
                yield i
    ];;

let multipesOf1 x = [ for i in 1 .. 10 do yield x * i ]
let multipesOf2 x = [ for i in 1 .. 10 -> x * i ]

multipesOf1 10;;
multipesOf2 11;;

// example of using list comprehension to compute primes
let primesUnder max =
    [
        for n in 1 .. max do
            let factorsOfN =
                [
                    for i in 1 .. n do
                        if n % i = 0 then
                            yield i
                ]
            if List.length factorsOfN = 2 then
                yield n
    ];;

primesUnder 100;;

// it's high time that we got along with list module functions
let testList = [1 .. 100]

List.length testList;;
// 100
List.head testList;;
// 1
List.tail testList;;
// [2..100]
List.head (List.tail (List.tail testList));;
// 3

List.exists ((=) 10) testList;;
// true

// if there's any element in list that satisfies (< 100 x) (i.e. 100 < x)
List.exists ((<) 100) testList;;
// false

// reverse a list
List.rev testList;;

let greaterThan x y = y > x
    
// note here is 'tryFind' rather than 'tryfind' in book
List.tryFind (greaterThan 50) testList;;
// returns Some 51

List.tryFind (greaterThan 100) testList;;
// return None because nothing can be greater than 100 in this list

// zip 2 lists to produce a list of tuples
//     whose values are extracted from the 2 given lists
List.zip testList [ for i in 1..100 -> i * 2];;

// hello, filter!
List.filter (greaterThan 90) testList;;

// break a list into two using a given condition
List.partition (greaterThan 49) testList;;

let isMultipleOf5 x = (x % 5 = 0)

let multOf5, nonMultipleOf5 = List.partition isMultipleOf5 [1..15];;

// we'll find our old friends: map and fold!

List.map square [1..10];;
[ for i in 1..10 -> square i];;
// [1; 4; 9; .. 100]

let insertCommas (acc:string) item = acc + ", " + item;;
List.reduce insertCommas ["Jack"; "Jill"; "Jim"; "Joe"; "Jane"];;

let addAccToListItem = (+);;

List.fold addAccToListItem 0 [1;2;3];;
// 6

let countVowels (str : string) =
    let charList = List.ofSeq str

    let accFunc (As, Es, Is, Os, Us) letter =
        match letter with
        | 'a' -> (As+1, Es, Is, Os, Us)
        | 'e' -> (As, Es+1, Is, Os, Us)
        | 'i' -> (As, Es, Is+1, Os, Us)
        | 'o' -> (As, Es, Is, Os+1, Us)
        | 'u' -> (As, Es, Is, Os, Us+1)
        | _ -> (As, Es, Is, Os, Us)

    List.fold accFunc (0,0,0,0,0) charList;;

countVowels "The quick brown fox jumps over the lazy dog";;

// note F# also have List.reduceBack and List.foldBack, just like what foldr does in Haskell
//     I'll skip this part because I think it'll be easy to figure out.

// List.iter is predominately used for eval functions that have side effects
let printNumber x = printfn "Printing %d" x
List.iter printNumber [1..5];;

#quit;;
