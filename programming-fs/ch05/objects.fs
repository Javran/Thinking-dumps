// note this file only works in interactive mode

// common methods

// ToString
type PunctuationMark =
    | Period
    | Comma
    | QuestionMark
    | ExclamationPoint

    override this.ToString() =
        match this with
        | Period -> "Period (.)"
        | Comma -> "Comma (,)"
        | QuestionMark -> "QuestionMark (?)"
        | ExclamationPoint -> "ExclamationPoint (!)"

[ Period
; Comma
; QuestionMark
; ExclamationPoint ]
    // %O will call ToString() to get the result
    // %A will instead print "Period" / "Comma" / "QuestionMark" / "ExclamationPoint"
    |> Seq.iter (printfn "%O");;

// GetHashCode

[ "alpha" ; "bravo" ]
    |> Seq.iter (fun x -> printfn "%A" (x.GetHashCode()))

// Equal

// one thing to remember: if Equals is overriden, 
//     GetHashCode need to ensure when two objs are equal, so will their hash code be

// GetType
let stringType = "".GetType();;
// System.String

stringType.AssemblyQualifiedName;;

// object equality

// for primitive type, each byte in the data is compared

printfn "%A,%A" (1=2) (2.123=2.123)
// false, true
printfn "%A" (nan=nan);;
// false ; it's a legal number that cannot met predication "x = x"
// some very special case ...

// for referential types
type ClassType(x : int) =
    member this.Value = x

let x, y = new ClassType(42), new ClassType(42)
printfn "%A,%A,%A" (x=y) (x=x) (y=y)
// false, true, true
// actually here "x=y" means both x and y should point to the same obj

// the behavior can be customized by overriding "Equals"(also remember to override "GetHashCode")

type ClassType2(x:int) =
    member this.Value = x
    override this.Equals(o:obj) =
        match o with
        | :? ClassType2 as other -> (other.Value = this.Value)
        | _ -> false
    override this.GetHashCode() = x

let a1,a2,a3 = new ClassType2(31), new ClassType2(31), new ClassType2(10000)

printfn "%A,%A,%A" (a1=a2) (a2=a3) (a3=a1)
// true, false, false

#quit;;
