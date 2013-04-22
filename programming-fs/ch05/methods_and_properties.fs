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
            f()
        with
        | _ as ex ->
            printfn "Failed: %s" ex.Message

    Seq.iter (fun _ -> ignore <| new RareType()) [1..5];;

#quit;;
