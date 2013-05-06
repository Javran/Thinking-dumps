// note this file only works in interactive mode

// mutable function values

type Widget =
    | Sprocket of string * int
    | Cog of string * float

let mutable generateWidget =
    let count = ref 0
    (fun () -> 
        incr count
        Sprocket( "Model A1", !count ));;

generateWidget();;
generateWidget();;

// switch impl

generateWidget <-
    let count = ref 0
    (fun () ->
        incr count
        Cog( (sprintf "Version 0x%x" !count), 0.0 ));;

generateWidget();;
generateWidget();;

// lazy programming

// values are not evaluted until they are forced.
// so we can sometimes save time & memory(don't need to pay for unnecessary calculations)

type LazyBinTree<'a> =
    | Node of 'a * LazyBinTree<'a> Lazy * LazyBinTree<'a> Lazy
    | Empty

let rec lazyMap f (tree : LazyBinTree<'a>) =
    // TODO: why here we have to write "match" rather than "function"
    match tree with
    | Empty -> Empty
    | Node(x, l, r) ->
        Node(
            f x,
            // this line will fail if we use "function" instead of "match" above ...
            lazy( lazyMap f (l.Value) ),
            lazy( lazyMap f (r.Value) ))

// skipping next part: abstracting data access, since nothing new there ...

#quit;;
