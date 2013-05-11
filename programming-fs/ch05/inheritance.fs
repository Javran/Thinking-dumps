// note this file only works in interactive mode

type BaseClass =
    val m_field1 : int

    new(x) = { m_field1 = x }
    member this.Field1 = this.m_field1

type ImplicitDerived(field1, field2) =
    inherit BaseClass(field1)
    // why 'let' rather than 'val' here?
    let m_field2 : int = field2
    member this.Field2 = m_field2

type ExplicitDerived =
    inherit BaseClass

    val m_field2 : int

    // why curly bracket?
    new (field1, field2) = {
        inherit BaseClass(field1)
        m_field2 = field2
    }

    member this.Field2 = this.m_field2

// little confused here: 
// * difference between "val"/"let"?
// * why things declared by "let" not shown in the output?
// * why should we use curly bracket when using "inherit"?

type SandWich() =
    abstract Ingredients : string list
    default this.Ingredients = []

    abstract Calories : int
    default this.Calories = 0

type BLTSandWich() =
    inherit SandWich()

    override this.Ingredients = 
        [ "Bacon"
        ; "Lettuce"
        ; "Tomato" ]
    override this.Calories = 330

type TurkeySwissSandWich() =
    inherit SandWich()

    override this.Ingredients =
        [ "Turkey"
        ; "Swiss" ]
    override this.Calories = 330

type BLTWithPickleSandWich() =
    inherit BLTSandWich()

    // use "base" to access the base class
    override this.Ingredients = "Pickles" :: base.Ingredients
    override this.Calories = 50 + base.Calories

let inheritTest1 =
    let showSandWich (s:SandWich) =
        printfn "Ingredients: %A" s.Ingredients
        printfn "Calories: %d" s.Calories

    showSandWich <| new SandWich()
    showSandWich <| new BLTSandWich()
    showSandWich <| new TurkeySwissSandWich()
    showSandWich <| new BLTWithPickleSandWich()

// define an abstract class
[<AbstractClass>]
type AbstractClass() = 
    member this.Alpha() = true
    abstract member Bravo : unit -> bool;;

// implement the abstract method
// please refer to:
// http://msdn.microsoft.com/en-us/library/dd483471.aspx
type Foobar() =
    inherit AbstractClass()
    
    override this.Bravo () = true;;

[<Sealed>]
type SealedClass () =
    member this.Alpha() = true;;

// static upcase: derived -> base
[<AbstractClass>]
type Animal () =
    abstract member Legs : int

[<AbstractClass>]
type Dog () =
    inherit Animal()

    abstract member Description : string
    override this.Legs = 4

type Pomeranian () =
    inherit Dog()
    override this.Description = "Furry"

let castTest =
    let steve = new Pomeranian()
    let steveAsDog = steve :> Dog
    let steveAsAnimal = steve :> Animal
    let steveAsObject = steve :> obj
    printfn "%s,%s" (steve.Description) (steveAsDog.Description)
    // Furry,Furry
    // dynamic cast: cannot be checked statically by compiler, e.g. base -> derived
    let steveAsDog1 = steveAsObject :?> Dog
    printfn "%s,%d" (steveAsDog1.Description) (steveAsDog1.Legs)
    // Furry, 4
    try
        let x = steveAsObject :?> string
        printfn "%s" x
    with
    | _ as ex ->
        printfn "Error: %s" ex.Message

// pattern matching against types
let whatIs (x : obj) =
    match x with
    | :? string as s ->
        printfn "x is a string: \"%s\"" s
    | :? int as i ->
        printfn "x is an int %d" i
    | :? list<int> as l ->
        printfn "x is an int list '%A'" l
    | _ ->
        printfn "x is a '%s'" <| x.GetType().Name;;

whatIs <| [1..5];;
whatIs <| "Rosebud";;
whatIs <| new System.IO.FileInfo(@"c:\config.sys");;
whatIs <| new Pomeranian();;

#quit;;
