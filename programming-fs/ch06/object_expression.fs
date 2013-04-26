// note this file only works in interactive mode

// object expression for interfaces
open System.Collections.Generic

type Person =
    { First : string; Last : string }
    override this.ToString () = sprintf "%s, %s" this.Last this.First

let people =
    new List<_>(
        [| 
            { First = "Jomo"; Last = "Fisher" };
            { First = "Brian"; Last = "McNamara" }; 
            { First = "Joe"; Last = "Pamer" }
        |] )

let printPeople () =
    Seq.iter (fun person -> printfn "\t%s" (person.ToString())) people

printfn "Initial ordering:"
printPeople()

printfn "Sorting by first name:"
people.Sort(
    {
        new IComparer<Person> with
            member this.Compare(l,r) =
                match l,r with
                | _ when l.First > r.First ->
                    1
                | _ when l.First < r.First ->
                    -1
                | _ -> 0
    } )

printPeople()

printfn "Sorting by last name:"
people.Sort(
    {
        new IComparer<Person> with
             member this.Compare(l,r) =
                match l,r with
                | _ when l.Last > r.Last ->
                    1
                | _ when l.Last < r.Last ->
                    -1
                | _ -> 0
    } )

printPeople()

// object expression for derived classes
[<AbstractClass>]
type SandWich () =
    abstract Ingredients : string list
    abstract Calories : int

let lunch =
    {
        new SandWich() with
            member this.Ingredients = ["Peanutbutter"; "Jelly"]
            member this.Calories = 400
    };;

lunch.Ingredients;;
// Peanutbutter and Jelly

#quit;;
