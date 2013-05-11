namespace FSCollectionExtensions

open System.Collections.Generic

module List =
    let rec skip n ls =
        match n,ls with
        | _, []         -> []
        | 0, ls         -> ls
        | n, hd :: tl   -> skip (n-1) tl

module Seq =
    let rec rev (s:seq<'a>) =
        let stack = new Stack<'a>()
        s |> Seq.iter stack.Push
        seq {
            while stack.Count > 0 do
                yield stack.Pop()
        }
