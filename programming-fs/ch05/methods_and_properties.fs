// note this file only works in interactive mode
[<Measure>]
type ml

type WaterBottle1() =
    // is this a variable bound to the scope of "WaterBottle1"?
    let mutable m_amount = 0.0<ml>
    
    member this.Empty = (m_amount = 0.0<ml>)
    member this.Amount with get() = m_amount
                        and set newAmt = m_amount <- newAmt;;

let testBottle1 =
    let b = WaterBottle1()
    printfn "Is bottle empty? %b" b.Empty
    // true
    b.Amount <- 10.0<ml>
    printfn "Bottle amount: %A" b.Amount
    // 10 ml
    printfn "Is bottle empty? %b" b.Empty
    // false

// F# can make getter and setter for us
type WaterBottle2() =
    member this.Empty = (this.Amount = 0.0<ml>)
    member val Amount = 0.0<ml>
        with get, set

let testBottle2 =
    let b = WaterBottle2()
    printfn "Is bottle empty? %b" b.Empty
    // true
    b.Amount <- 10.0<ml>
    printfn "Bottle amount: %A" b.Amount
    // 10 ml
    printfn "Is bottle empty? %b" b.Empty
    // false

// setting properties in the constructor
// make this part toggable by wraping it inside a function
open System.Windows.Forms
let testSettingPropInConstrutor () =

    // attempt #1
    let f1 = new Form()
    f1.Text     <- "Window Title"
    f1.TopMost  <- true
    f1.Width    <- 640
    f1.Height   <- 480
    ignore <| f1.ShowDialog()

    // attempt #2
    let mkFormWithTitle title =
        new Form
            ( Text      = title
            , TopMost   = true
            , Width     = 640
            , Height    = 480)

    ignore <| (mkFormWithTitle "WindowTitle - 2").ShowDialog();;

// uncomment the following line for corresponding examples
// testSettingPropInConstrutor ();;

// methods

// seems no way to define mutable members by implicit constructor ...
type Television = 
    val mutable m_channel : int
    val mutable m_turnedOn : bool

    new () = { m_channel = 3; m_turnedOn = true}

    member this.TurnOn () =
        printfn "Turning on ..."
        this.m_turnedOn <- true

    member this.TurnOff () =
        printfn "Turnning off ..."
        this.m_turnedOn <- false

    member this.ChangeChannel (newChannel : int) =
        if this.m_turnedOn = false
            then failwith "Cannot change channel, the TV is not on."
        printfn "Changing channel to %d ..." newChannel
        this.m_channel <- newChannel
    member this.CurrentChannel = this.m_channel

let testTV =
    let tv = new Television()

    tv.TurnOff()
    tv.TurnOn()
    tv.ChangeChannel 0
    printfn "TV channel: %d" tv.CurrentChannel;;

// it's great that method can be partially applied,
//     but the best practice is to use tuple to pass argument
//     note when referenced from other .Net languages, the argument will be "flatten"

// implicit constructor by using "()", which were ignored, might not compile.
type Adder () =
    member this.AddTwoParams = (+)
    member this.AddTwoTupledParams(x,y) = this.AddTwoParams x y;;

let testAdder =
    let a = new Adder()
    let b = a.AddTwoParams 1

    printfn "%d" (b 2)
    // 1+2=3
    printfn "%d" (a.AddTwoTupledParams(3,4))
    // 3+4=7

// static members
// "static" keyword, without self identifier

type StaticMethodTest() =
    static member StaticMember () = 5;;

StaticMethodTest.StaticMember();;
// 5

// a rare type that can only be initialized for limited times
type RareType () =
    static let mutable m_numLeft = 2

    do
        if m_numLeft <= 0
            then failwith "No more left!"
        m_numLeft <- m_numLeft - 1
        printfn "Initialized a rare type, only %d left!" m_numLeft
    static member NumLeft () = m_numLeft

let testRareType =
    let mightFail f =
        try
            ignore <| f()
        with
        | _ as ex ->
            printfn "Failed: %s" ex.Message

    // try to initialize the list for 5 times
    Seq.iter (fun _ -> ignore <| mightFail (fun () -> new RareType())) [1..5];;

// please refer to :
// http://stackoverflow.com/questions/16162722/can-we-have-more-generic-type-to-shorten-this-code-in-f/
// http://msdn.microsoft.com/en-us/library/dd548047.aspx
open LanguagePrimitives

type BitCounter =
    static member inline CountBitsWithRangedMask x upBound =
        seq { for i = GenericZero to upBound do yield GenericOne <<< i }
            |> Seq.map ((&&&) x)
            |> Seq.filter ((<>) GenericZero)
            |> Seq.length

    static member CountBits (x : int16) =
        BitCounter.CountBitsWithRangedMask x 15
    
    static member CountBits (x : int) =
        BitCounter.CountBitsWithRangedMask x 31

    static member CountBits (x : int64) =
        BitCounter.CountBitsWithRangedMask x 63

// accessibility modifiers

// public: visible from anywhere
// private: only visible among class members
// internal: visible inside assembly

open System

// hiding main constructor
type internal Ruby private(shininess, carats) =
    let mutable m_size = carats
    let mutable m_shininess = shininess

    // polishing increases shininess but decreases size
    member this.Polish() =
        this.Size <- this.Size - 0.1
        m_shininess <- m_shininess + 0.1

    // getter & setter
    member public this.Size with get() = m_size
    member private this.Size with set newSize = m_size <- newSize

    member this.Shininess = m_shininess

    public new() =
        let rng = new Random()
        let s = float ( rng.Next() % 100 ) * 0.01
        let c = float ( rng.Next() % 16 ) + 0.1
        new Ruby(s, c)

    public new(carats) =
        let rng = new Random()
        let s = float ( rng.Next() % 100) * 0.01
        new Ruby(s,carats)

    override this.ToString() =
        sprintf "Ruby: Shininess: %f, Size: %f" this.Shininess this.Size


let testRuby =
    let r1 = new Ruby()
    printfn "%A" r1
    r1.Polish()
    printfn "%A" r1
    
    let r2 = new Ruby(0.5)
    printfn "%A" r2
    r2.Polish()
    printfn "%A" r2

// accessibility modifiers on module values
open System.IO
open System.Collections.Generic

module Logger =
    let mutable private m_filesToWriteTo = new List<string>()
    let AddLogFile(filePath) = m_filesToWriteTo.Add( filePath )
    let LogMessage(message:string) =
        Seq.iter 
            (fun filePath ->
                use file = new StreamWriter(filePath, true)
                file.WriteLine(message)
                file.Close())
            m_filesToWriteTo

Logger.LogMessage "Nothing"
Logger.AddLogFile "test.log"
Logger.LogMessage "New log file: test.log"

#quit;;
