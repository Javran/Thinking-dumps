// note this file only works in interactive mode

// cons operator: `::`

let testCons =
    let x = [2; 3; 4]
    1 :: x;;
// [1; 2; 3; 4]
//
// the actual structure might be: (a linked list)
// 1 -> 2 -> 3 -> 4 -> []

// append function: `@`

let testAppend =
    let x = [2;3;4]
    let y = [5;6;7]

    1 :: x @ y;;
// [1..7]
// append is potentially more expensive because the first list is cloned

(* 
    Removes consecutive duplicate numbers:
        [1;1;2;2;3;4] => [1;2;3;4]
*)
let testCase = [1;1;2;2;3;4]

let removeConsecutiveDupes1 lst =
    let foldFunc acc item =
        let lastNumber, dupesRemoved = acc
        match lastNumber with
        | Some(c) when c = item ->
            Some(c), dupesRemoved // ignore 'c' because the same value has been seen in list
        | Some(c) ->
            Some(item), dupesRemoved @ [item]
        | None ->
            Some(item), [item] // construct the list because it's the first element

    let (_, dupesRemoved) = List.fold foldFunc (None,[]) lst
    dupesRemoved

// fast impl
let removeConsecutiveDupes2 lst =
    let f item acc =
        match acc with
        | [] ->
            [item]
        | hd :: tl when hd <> item ->
            item :: acc // this guarantees that item <> hd
        | _ -> // a duplicate of the first element, skipping
            acc
    List.foldBack f lst []

removeConsecutiveDupes1 testCase;;
removeConsecutiveDupes2 testCase;;
// [1;2;3;4]

#quit;;
