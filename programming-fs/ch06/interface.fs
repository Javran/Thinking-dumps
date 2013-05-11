// note this file only works in interactive mode

type Tastiness =
    | Delicious
    | SoSo
    | TrySomethingElse

type IComsumable =
    abstract Eat : unit -> unit
    abstract Tastiness : Tastiness

type Apple () =
    interface IComsumable with
        member this.Eat() = printf "Tasty!"
        member this.Tastiness = Delicious

type CardboardBox () = // WTF? ...
    interface IComsumable with
        member this.Eat() = printf "Yuck!"
        member this.Tastiness = TrySomethingElse

// using interface

let apple = new Apple()

// the line below causes error
// apple.Tastiness;;

(apple :> IComsumable).Tastiness;;

// defining interface

// define a type inferred to be an interface
type IDoStuff =
    abstract DoThings : unit -> unit;;

// define an interface explicitly
type IDoStuffToo =
    interface
        abstract member DoThings : unit -> unit
    end

// interfaces can be inherited as well
type IDoMoreStuff =
    inherit IDoStuff

    abstract DoMoreStuff : unit -> unit

type Bar() =
    interface IDoStuff with
        member this.DoThings () = printfn "Stuff getting done ..."

    interface IDoMoreStuff with
        member this.DoMoreStuff () = printfn "More stuff getting done ..."

#quit;;
