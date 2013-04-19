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

printfn "%A,%A,%A" (a1=a2) (a2=a3) (a3=a1);;
// true, false, false

// generated equality

// tuples are considered equal if they have same number of elements and each element is equal
let x = (1, 'a', "str");;
printfn "%A,%A,%A"
    (x=x) // true
    (x = (1, 'a', "str")) // true
    ((x,x) = (x, (1, 'a', "str"))) // true

// tuple with different number of elements might not pass the compiler,
// for example, the next line causes error rather than returning a value
// I guess "=" requires both of its argument are of same type
// (x = (1, 'a', "str", ()));;

// check the type signature
(=);;
// 'a -> 'a -> bool

// thus: equality includes type equality, which is checked at complie time.

// record equality = all fields are equal
type RecType = { Field1 : string; Field2 : float }

let testRecEqu =
    let x,y,z =
        { Field1 = "abc"; Field2 = 3.5 },
        { Field1 = "abc"; Field2 = 3.5 },
        { Field1 = "XXX"; Field2 = 0.0 }

    printfn "%A,%A,%A" (x=y) (y=z) (x=z) // true, false, false

// discriminated union: it might be verbose to give an exact definition of equality,
// but what it means is obvious :)

type DUType =
    | A of int * char
    | B

let testDUnion =
    let x,y,z =
        A (1,'k'),
        A (1,'k'),
        B

    printfn "%A,%A,%A" (x=y) (y=z) (x=z) // true, false, false

// we can use referential equality as well:
[<ReferenceEquality>]
type RefDUType =
    | A of int * char
    | B

let testRefDUnion =
    let x,y,z =
        A (1,'k'),
        A (1,'k'),
        B

    printfn "%A,%A,%A" (x=y) (y=z) (x=z) // false, false, false

#quit;;
