// note this file only works in interactive mode

open System.Collections.Generic

type CoffeeCup(amount : float) =
    let mutable m_amountLeft = amount
    let mutable m_interestedParties = List<(CoffeeCup -> unit)>()

    member this.Drink(amount) =
        printfn "Drinking %.1f..." (float amount)
        m_amountLeft <- max (m_amountLeft - amount) 0.0
        if m_amountLeft <= 0.0
            then this.LetPeopleKnowIamEmpty()

    member this.Refil(amountAdded) =
        printfn "Coffee Cup refilled with %.1f" (float amountAdded)
        m_amountLeft <- m_amountLeft + amountAdded

    member this.WhenYouAreEmptyCall(func) =
        m_interestedParties.Add(func)

    member private this.LetPeopleKnowIamEmpty() =
        printfn "Uh ho, I'm empty! Letting people know..."
        for interestedParty in m_interestedParties do
            interestedParty(this)

let testCup =
    let cup = new CoffeeCup(100.0)

    cup.WhenYouAreEmptyCall(
        fun cup ->
            printfn "Thanks for letting me know ..."
            cup.Refil(50.0))

    cup.Drink(75.0)
    cup.Drink(75.0)

printfn "";;

// defining delegates

let printAndAdd x y =
    printfn "x = %d, y = %d" x y
    x + y

// really confusing ... why not int -> int -> int
// if "int -> int -> int" is used, the compiler will say that:
// "Delegate specifications must not be curried types."
// ... if it helps.
type DelegateType = delegate of int * int -> int

let delegateValue1 =
    new DelegateType(
        fun x y ->
            printfn "x = %d, y = %d" x y
            x + y)

printAndAdd 1 2;;
delegateValue1.Invoke(1,2);;

type IntDelegate = delegate of int -> unit

type ListHelper =
    static member ApplyDelegate (l : int list, d : IntDelegate) =
        l |> List.iter (fun x -> d.Invoke(x))

// construct the delegate explicitly
ListHelper.ApplyDelegate([1..10], new IntDelegate( printf "[%d]" ))
printfn "";;

// construct the delegate implicitly
// why partial function does not work here?
// ListHelper.ApplyDelegate([1..10], (printf "<%d>"))
ListHelper.ApplyDelegate([1..10], (fun x -> printf "<%d>" x))
printfn "";;

// combining delegates
type LogMessage = delegate of string -> unit

let printToConsole =
    LogMessage(fun msg -> printfn "Logging to console: %s ..." msg)

let appendToLogFile =
    LogMessage(fun msg ->
                    printfn "Logging to file: %s ..." msg
                    printfn "Pretending that I've appended it to a file :)")
    
let doBoth = LogMessage.Combine( printToConsole, appendToLogFile );;
// type for doBoth is just System.Delegate, we need to convert it to a LogMessage
let typedDoBoth = doBoth :?> LogMessage

// one shot with two delegates triggered!
typedDoBoth.Invoke("Message!");;

#quit;;
