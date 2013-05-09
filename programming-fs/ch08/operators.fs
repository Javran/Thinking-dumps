// note this file only works in interactive mode

// operator overloading: adding new meaning to an existing operator

[<Measure>]
type ml

type Bottle(capacity : float<ml>) =
    new() = new Bottle(0.0<ml>)

    member this.Volume = capacity

    static member (+) ((lhs : Bottle), rhs) =
        new Bottle(lhs.Volume + rhs)

    static member (-) ((lhs : Bottle), rhs) =
        new Bottle(lhs.Volume - rhs)
        

    // actually its '-', i.e. its unary negation version 
    // seems when an operator has a leading "~", it will refer to its unary version
    // e.g. ~+, ~-, ~! ...
    // actually F# allows you to define arbitary symbolic operators...
    // FYI: +-+ will becomes "op_PlusMinusPlus"
    static member (~-) (rhs : Bottle) =
        // seems here <1> stands for no measurements,
        //     so the measurement unit of result can be kept the same as that of rhs.Volume's
        new Bottle(rhs.Volume * (-1.0<1>))

    override this.ToString() =
        sprintf "Bottle(%.1fml)" (float capacity)

let half = new Bottle(500.0<ml>);;
// 500ml
half + 500.0<ml>;;
// 1000ml
half - 400.0<ml>;;
// 100ml
- half;;
// -500ml

type Person =
    | Boy of string
    | Girl of string
    | Couple of Person * Person
    static member (+) (lhs, rhs) =
        match lhs,rhs with
        | Couple(_),_
        | _,Couple(_) ->
            failwith "There's a crowd!"
        | _ -> Couple(lhs, rhs)

Boy("Dick") + Girl("Jane");;

// here I've wasted much time figuring out how to convert between floats with/without UoM
// I think I'd better never use it unless I know exactly how to convert between these types
// despite that, that's great that Seq.fold works as what we've expected
Seq.fold (+) (new Bottle()) [1.0<ml>; 2.0<ml>; 3.0<ml>];;

open System

type Year(year : int) =
    member this.Item (idx : int) =
        // seems "[]" will call this method implicitly
        if idx < 1 || idx > 365
            then failwith "Invalid day range"

        let dateStr = sprintf "1-1-%d" year
        DateTime.Parse(dateStr).AddDays(float(idx - 1))

let eightyTwo = new Year(1982)
let specialDay = eightyTwo.[171];;
specialDay.Month, specialDay.Day, specialDay.DayOfWeek;;
// 1982-06-20

// non-integer indexer
type Year2(year : int) =
    member this.Item (month : string, day : int) =
        let monthIdx =
            match month.ToUpper() with
            | "JAN" ->  1 | "FEB" ->  2 | "MAR" ->  3
            | "APR" ->  4 | "MAY" ->  5 | "JUN" ->  6
            | "JUL" ->  7 | "AUG" ->  8 | "SEP" ->  9
            | "OCT" -> 10 | "NOV" -> 11 | "DEC" -> 12
            | _ -> failwithf "Invalid month [%s]" month

        let dateStr = sprintf "1-1-%d" year
        DateTime.Parse(dateStr).AddMonths(monthIdx - 1).AddDays(float(day - 1))

(new Year2(2007)).["Apr",7];;
// 2007-04-07

// read/write indexer

open System.Collections.Generic

type WordBuilder(startingLetters : string) =
    let m_letters = new List<char>(startingLetters)

    member this.Item
        with get idx =
            m_letters.[idx]
        and set idx c =
            m_letters.[idx] <- c

    member this.Word =
        new string(m_letters.ToArray())

let wb = new WordBuilder("abd")
wb.[0];;
// 'a'
wb.[2] <- 'c'
wb.Word;;

// adding slices

type TextBlock(text : string) =
    let words = text.Split([|' '|])

    member this.AverageWordLength =
        // the code in the book is wrong
        words |> Array.map (fun x -> float x.Length ) |> Array.average

    // this method is called implicitly when use something like inst.[a..b]
    member this.GetSlice(lowerBound : int option, upperBound : int option) =
        let words =
            match lowerBound, upperBound with
            | Some(lb), Some(ub) ->
                words.[lb..ub]
            | Some(lb), None ->
                words.[lb..]
            | None, Some(ub) ->
                words.[..ub]
            | None, None ->
                words.[*]
        words

let text = "The quick brown fox jumps over the lazy dog."

let tb = new TextBlock(text)

tb.AverageWordLength;;
// average word length

tb.[*];;
// everything

tb.[..2];;
// first 3 words

tb.[1..1];;
// the second word wrapped in list

tb.[5..];;
// skipping the first 5 words

// two-dimensional slice
open System

type DataPoints(points : seq<int*int>) =
    member this.GetSlice(xlb,xub,ylb,yub) =
        // safe getter, always returns a valid value
        let getValue optType defaultValue =
            match optType with
            | Some(x) -> x
            | None -> defaultValue

        let minX = getValue xlb Int32.MinValue
        let maxX = getValue xub Int32.MaxValue

        let minY = getValue ylb Int32.MinValue
        let maxY = getValue yub Int32.MaxValue

        let inRange (x, y) =
            (minX <= x && x <= maxX &&
             minY <= y && y <= maxY)

        Seq.filter inRange points

let points =
    seq {
        for i = 0 to 99 do
            for j = 0 to 49 do
                yield (i, j)
    };;

let d = new DataPoints(points)
// a line ..
Seq.iter (printf "<%A>") d.[0..49,0..0]
printfn ""

Seq.iter (printf "[%A]") d.[0..2,45..49]
printfn ""

#quit;;
