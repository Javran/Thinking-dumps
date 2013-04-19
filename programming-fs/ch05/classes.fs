// note this file only works in interactive mode

type Point =
    // define fields
    val m_x : float
    val m_y : float

    // constructor 1
    new (x,y) = { m_x = x; m_y = y }

    // constructor 2
    new () = { m_x = 0.0; m_y = 0.0 }

    member this.Length =
        let sqr x = x * x

        sqrt <| sqr this.m_x + sqr this.m_y

let p1 = new Point(1.0, 1.0)
let p2 = new Point()

printfn "%A,%A" p1 p2;;

// explicit construction
open System

type Point2 =
    val m_x : float
    val m_y : float

    // parse a string
    new (text : string) as this =
        if text = null then
            raise <| new ArgumentException("text")

        let parts = text.Split(',')
        let (successX, x) = Double.TryParse(parts.[0])
        let (successY, y) = Double.TryParse(parts.[1])

        if not successX || not successY then
            raise <| new ArgumentException("text")

        { m_x = x; m_y = y}
        
        then
            // then?! ... WTF
            printfn 
                "Initialized to [%f,%f]"
                this.m_x
                this.m_y

new Point2("1.0,2.0");;
// Initialized to [1.0,2.0]

// implicit construction
//     the way function programming would prefer

type Point3(x : float, y : float) =
    let length =
        let sqr x = x * x
        sqrt <| sqr x + sqr y
    // do ?!
    do printfn "Initialized to [%f,%f]" x y

    member this.X = x
    member this.Y = y
    member this.Length = length

    // custom constructors here must call 'main' constructor
    new() = new Point3(0.0, 0.1)

    new(text:string) =
        if text = null then
            raise <| new ArgumentException("text")
            
        let parts = text.Split(',')
        let (successX, x) = Double.TryParse(parts.[0])
        let (successY, y) = Double.TryParse(parts.[1])

        if not successX || not successY then
            raise <| new ArgumentException("text")

        new Point3(x,y)
// 3 members, 3 constructors
new Point3(1.0,1.0)
// Initialized to [1.0,1.0]
new Point3()
// Initialized to [0.0,0.0]
new Point3("1.0,10.0")
// Initialized to [1.0,10.0]

// generic classes

// F# use 'a, 'b, 'c ... as convension for generic types

type Arrayify<'a>(x : 'a) =
    let infiniteSeqOf x = seq { while true do yield x }
    let arrayRepeat x times = Seq.toArray <| Seq.take times (infiniteSeqOf x)

    member this.EmptyArray : 'a[] = arrayRepeat x 0 
    member this.ArraySize1 : 'a[] = arrayRepeat x 1
    member this.ArraySize2 : 'a[] = arrayRepeat x 2
    member this.ArraySize3 : 'a[] = arrayRepeat x 3

let arrayifyTuple = new Arrayify<int * int>( (10,27) );;
arrayifyTuple.ArraySize3;;
// [| (10,27); (10,27); (10;27) |]

// compiler can infer types for us
let infered = new Arrayify<_>( "string" );;
// Arrayify<string>


// we can make records and discriminated unions as generic types
// discriminated unions
type GenDU<'a> =
    | Tag1 of 'a
    | Tag2 of string * 'a list

Tag1 "aaa";;
// GenDu<string>
Tag2 ("11",[(1,1)]);;
// GenDu<int * int>

type GenRec<'a,'b> = { Field1 : 'a; Field2 : 'b };;

let x = { Field1 = "Blue"; Field2 = (1,2) };;
// GenRec<string,(int * int)>

#quit;;
