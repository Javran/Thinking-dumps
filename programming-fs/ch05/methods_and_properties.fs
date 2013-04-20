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


#quit;;
