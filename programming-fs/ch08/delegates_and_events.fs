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

// defining delegates

// working :3

#quit;;
