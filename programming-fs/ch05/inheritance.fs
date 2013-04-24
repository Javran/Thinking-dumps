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

let inheritTest1 =
    let showSandWich (s:SandWich) =
        printfn "Ingredients: %A" s.Ingredients
        printfn "Calories: %d" s.Calories

    showSandWich <| new SandWich()
    showSandWich <| new BLTSandWich()
    showSandWich <| new TurkeySwissSandWich()

#quit;;
