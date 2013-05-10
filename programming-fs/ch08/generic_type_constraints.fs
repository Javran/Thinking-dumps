// note this file only works in interactive mode

// we can add some constraints on generic type

open System
open System.Collections.Generic

exception NotGreaterThanHead

type GreaterThanList<'a when 'a :> IComparable<'a> >(minValue : 'a) =
    
    let m_head = minValue

    let m_list = new List<'a>()

    do m_list.Add(minValue)

    member this.Add(newItem : 'a) =
        let ic = newItem :> IComparable<'a>

        if ic.CompareTo(m_head) >= 0
            then m_list.Add( newItem )
            else raise NotGreaterThanHead

    member this.Items = m_list :> seq<'a>
        
let testGreaterThanList =
    let ls = new GreaterThanList<_>(10)

    ls.Add(11)
    ls.Add(12)
    try
        ls.Add(0)
    with ex ->
        printfn "Error: %s" ex.Message

// I'd like to skip the rest of this part ...

#quit;;
