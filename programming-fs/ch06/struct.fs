// note this file only works in interactive mode

// two ways of constructing a struct:
[<Struct>]
type StructPoint (x:int, y:int) =
    member this.X = x
    member this.Y = y

type StructRect (top:int, bottom:int, left:int, right:int) =
    struct
        member this.Top = top
        member this.Bottom = bottom
        member this.Left = left
        member this.Right = right

        override this.ToString () =
            sprintf "[%d, %d, %d, %d]" top bottom left right
    end

// sometimes this part might fail because of a damn "could not load type" error
// I won't waste my time here since it is definitely not my fault
let rec testRun () =
    try
        let sp = new StructPoint(1,2)
        printfn "%d,%d" sp.X sp.Y
        let sr = new StructRect(1,1,1,1)
        printfn "%A" sr
    with
    | _ as ex ->
        testRun()

testRun()

let equTest =
    let x = new StructPoint(1,20)
    let y = new StructPoint(1,20)
    x = y;;
    // true

[<Struct>]
type BookStruct (title:string, pages:int) =
    member this.Title = title
    member this.Pages = pages

    override this.ToString() =
        sprintf "Title: %A, Pages: %d" this.Title this.Pages

let bookTest =
    let book1 = new BookStruct("Philosopher's Stone", 309)
    let namelessBook = new BookStruct()
    printfn "%A" book1
    printfn "%A" namelessBook


// struct with mutable fields
[<Struct>]
type MPoint =
    val mutable X : int
    val mutable Y : int

    override this.ToString () =
        sprintf "{%d,%d}" this.X this.Y

// again for the same reason, struct sucks
try
    let mutable pt = new MPoint()
    pt.X <- 7
    pt.Y <- 8
    printf "%A" pt
with
| _ as ex -> ()

#quit;;
