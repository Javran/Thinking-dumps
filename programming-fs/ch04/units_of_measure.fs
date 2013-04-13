// note this file only works in interactive mode

[<Measure>]
type fahrenheit

[<Measure>]
type celsius

let printTemperature (temp : float<fahrenheit>) =
    printfn "%s" <|
        match temp with
        | _ when temp < 32.0<_>
            -> "Below freezing!"
        | _ when temp < 65.0<_>
            -> "Cold"
        | _ when temp < 75.0<_>
            -> "Just right!"
        | _ when temp < 100.0<_>
            -> "Hot!"
        | _
            -> "Scorching!";;

let seattle = 59.0<fahrenheit>;;
printTemperature seattle;;

let cambridge = 18.0<celsius>;;
// the line below will fail, which does not agree with measurement unit
// printTemperature cambridge;;

[<Measure>]
type m

1.0<m> * 1.0<m>;;
// goes to 1.0 m^2

1.0/1.0<m>;;
// 1 </m>

// unit of measurement can only contain static methods/properties

[<Measure>]
type s

// Hz can be defined in terms of 's'
[<Measure>]
type Hz = s ^ -1;;

5.0<Hz> = 1.0/0.2<s>;;

// conversion between fahrenheit and celsius
// (looks weird ...)
[<Measure>]
type far =
    static member ConvertToCel(x : float<far>) =
        (5.0<cel>/9.0<far>) * (x - 32.0<far>)
and [<Measure>] cel =
    static member ConvertToFar(x : float<cel>) =
        (9.0<far>/5.0<cel>*x) + 32.0<far>;;

far.ConvertToCel(100.0<far>);;
cel.ConvertToFar(100.0<cel>);;
    
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames
// see http://msdn.microsoft.com/en-us/library/hh289766.aspx

let flashlightIntensity = 80.0<cd>;;
let world'sLargestGoldNugget = 280.0<kilogram>;;

[<Measure>]
type rads;;

let halfPI = System.Math.PI * 0.5<rads>;;

// remove UoM before using sin
sin( float halfPI );;

// make type inference take care of measurement unit
let genericsSquare (x: float<_>) = x * x;;

genericsSquare 1.0<m/s>;;
genericsSquare 1.0<m^2>;;
genericsSquare 1.0;;

// represents a point respecting the unit of measurement
type Point< [<Measure>] 'u > (x: float<'u>, y: float<'u>) =
    member this.X = x
    member this.Y = y

    // remove unit
    member this.UnitlessX = float x
    member this.UnitlessY = float y

    member this.Length =
        let sqr x = x * x
        sqrt <| sqr this.X + sqr this.Y

    override this.ToString() =
        sprintf
            "{%f,%f}"
            this.UnitlessX
            this.UnitlessY

// I think it's a cool feature but kinda verbose
let p = new Point<m>(10.0<m>, 10.0<m>);;
p.Length;;

#quit;;
