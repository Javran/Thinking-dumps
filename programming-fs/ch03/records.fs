// note this file only works in interactive mode

type PersonRec =
    { First : string
    ; Last : string
    ; Age : int };;
    
// why PersonRec is not specified explicitly?
//     the answer might be: they can be infered by fields
let steve = { First = "Steve"; Last = "Holt"; Age = 17 };;

printfn "%s is %d years old" steve.First steve.Age;;

// cloning records

// note we can infer that ';' is optional in the records' definition
type Car =
    { Make : string
      Model : string
      Year : int
    };;

let thisYear's =
    { Make = "FSharp"
    ; Model = "Luxury Sedan"
    ; Year = 2012 }

// just clone thisYear's and override some fields
let nextYear's = { thisYear's with Year = 2013 }

printfn "%A" thisYear's;;
printfn "%A" nextYear's;;

// pattern matching

let allCoups =
    let allNewCars = [
        { Make = "Car1Make"
        ; Model = "Coup"
        ; Year = 2013} ;
        { Make = "Car2Make"
        ; Model = "Soup"
        ; Year = 2012} ]

    allNewCars
    |> List.filter
        (function 
            // note here we can only write interested fields
            //     "Year" and "Make" are omitted totally
            | { Model = "Coup" } -> true
            | _ -> false);;

// only one car is shown

// type inference
type Point = { X : float; Y : float };;

let distance pt1 pt2 =
    let square x = x * x
    sqrt <| square(pt1.X - pt2.X) + square(pt1.Y - pt2.Y);;

distance { X = 0.0; Y = 0.0} { X = 10.0; Y = 10.0 };;
// 14.1421...

// what if there's a conflict on the field names?
type Vector = { X : float; Y : float; Z : float }

// the line below might cause error
//let distance2 pt1 pt2 =
// we provide the type annotation instead
let distance2 (pt1 : Point) (pt2 : Point) =
    let square x = x * x
    sqrt <| square(pt1.X - pt2.X) + square(pt1.Y - pt2.Y);;
distance2 { X = 0.0; Y = 0.0} { X = 10.0; Y = 10.0 };;

// or use fully qualifying record fields
let origin = { Point.X = 0.0; Point.Y = 0.0 };;

// methods and properties

type Vector2 = 
    { X : float; Y : float; Z : float }
    member this.Length =
        sqrt <| this.X ** 2.0 + this.Y ** 2.0 + this.Z ** 2.0;;

let (v: Vector2) = { X = 10.0; Y = 20.0; Z = 30.0 };;
v.Length ;;
// 37.416...

#quit;
