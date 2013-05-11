// note this file only works in interactive mode

// tail recursion: save space for stack, preventing stack overflow exception
// if there are no additional instructions to execute,
//      then there is no need to store the instruction pointer on the stack
//      (and we don't need to store the current stack frame as well)

// an example of tail recursion
let factorial x =
    let rec tailRecursiveFactorial x acc =
        if x <= 1
            then acc
            else tailRecursiveFactorial (x-1) (acc*x)
    tailRecursiveFactorial x 1

// recall what we've learnt in SICP:
// this is a iteractive process,
//      which takes little memory space and run in linear time
// the iteractive process make (factorial x) * acc a constant throughout the process
//      so when x is reduced to 1, factorial 1 * acc = acc will be the final result
factorial 10;;
// 3628800

// a function is considered tail recursive if and only if
//      there is no work to be performed after a recursive call is executed

// accumulator pattern:
// * similiar to the idea suggested by SICP: add additional parameters,
//      whittle down the input while at the same time build up the answer

let rec mymap1 f ls =
    match ls with
    | [] ->
        []
    | hd::tl ->
        (f hd)::(mymap1 f tl)

let rec mymap2 f ls =
    let rec mymap2iter f ls acc =
        match ls with
        | [] ->
            acc
        | hd :: tl ->
            mymap2iter f tl ((f hd)::acc)
    mymap2iter f (List.rev ls) []

mymap1 ((+) 2) [1;2;3];;
mymap2 ((+) 2) [1;2;3];;
// [3;4;5]

// recursive function with branches

type BinTree<'a> =
    | Node of 'a * BinTree<'a> * BinTree<'a>
    | Empty

let rec iter f binTree =
    match binTree with
    | Empty -> ()
    | Node (x,l,r) ->
        f x
        iter f l
        iter f r

let theTree = 
    Node (
        1,
        Node (
            2,
            Empty,
            Empty),
        Node (
            3,
            Empty,
            Node (
                4,
                Empty,
                Empty)))



iter (printfn "[%A]") theTree

printfn ""

// continuation ... might be connected with "current continuation" in scheme...
// "continations are function values that represent the rest of the code to execute"

let printListRev ls =
    let rec printListRevTR ls cont =
        match ls with
        | [] ->
            // do the rest of stuffs remaining
            cont()
        | hd :: tl ->
            printListRevTR tl (fun () ->
                                    // do things here 
                                    printf "[%d]" hd
                                    // rest
                                    cont() )
    printListRevTR ls (fun () -> printfn " Done.")

printListRev [1;2;3;4];;

// try to write a tail recursive function on my own...

let treeIter f tree =
    let rec treeIterTR f tree cont =
        match tree with
        | Empty ->
            cont()
        | Node (x,l,r) ->
            f x
            treeIterTR f l (fun () ->
                                // when we've visited the left part of tree,
                                //      visit the right part.
                                treeIterTR f r cont)
    treeIterTR f tree (fun () -> printfn "") 
                                
treeIter (printf "<%A> ") theTree
// it works, the output should be <1>, <2>, <3>, <4> in order.

// cheers!

type ContinuationStep<'a> =
    | Finished
    | Step of 'a * (unit -> ContinuationStep<'a>)

let iter2 f binTree =

    let rec linearize binTree cont =
        match binTree with
        | Empty ->
            cont()
        | Node (x,l,r) ->
            // the "linearrize" process, first we need x, then we'll go through l and r
            // just like a lazy list ... we need to execute the lambda in order to get the next 'Step'
            Step(x, (fun () -> linearize l (fun () -> linearize r cont)))

    let steps = linearize binTree (fun () -> Finished)

    let rec processSteps step =
        match step with
        | Finished -> ()
        | Step(x, getNext) ->
            // Step stores what we need to deal with next and how can we do the rest of the tasks
            f x
            // execute "getNext" for the next element we need to take care of
            processSteps (getNext ())

    processSteps steps

iter2 (printf "<%A> ") theTree

printfn ""

#quit;;
