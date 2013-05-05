// note this file only works in interactive mode

// memoization

// most of the functions are pure (have no side effect)
//      the idea is to memorize results and save the time of doing identical calculation

open System.Collections.Generic

let memorize (f : 'a -> 'b) =
    let dict = new Dictionary<'a,'b> ()

    let memorizedFunc (input : 'a) =
        match dict.TryGetValue(input) with
        | true, x -> x
        | false, _ ->
            let answer = f input
            dict.Add(input, answer)
            answer
    memorizedFunc

#time;;

// emulates some long running function ...
let waitForXSecs x =
    System.Threading.Thread.Sleep( 1000 * x )
    x

// change the value to positive numbers to see the performance
let testSec = 0;;

waitForXSecs testSec;;

let memorizedF = memorize waitForXSecs

memorizedF testSec;;
memorizedF testSec;;

// memorizing recursive functions

let rec memorizedFib =
    let fib x =
        match x with
        | 0 | 1 -> 1
        | 2 -> 2
        | n ->
            // we should call the memorized function recursively
            //      rather than the original "fib"
            // TODO: I don't know why this line causes warning ...
            memorizedFib (n-1) + memorizedFib (n-2)
    memorize fib;;

let rec normalFib =
    function
    | 0 | 1 -> 1
    | x -> normalFib(x-1) + normalFib(x-2)

memorizedFib 40;
normalFib 40;;
// the latter one takes more time

#time "off";;

#quit;;
