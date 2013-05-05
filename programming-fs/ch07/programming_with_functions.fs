// note this file only works in interactive mode

// passing functions as parameters

let testFuncPassing =
    let sumUpThree a b c = a + b + c
    [1;2;3]
        |> Seq.map (fun x -> sumUpThree 1 2 x)
        |> Seq.iter (printfn "%A")

    [1;2;3]
        |> Seq.map (sumUpThree 1 2)
        |> Seq.iter (printfn "%A")
    // 4,5,6

// note passing functions as parameters sometimes might obfuscate code,
//      please always take care of readability

// eliminating redundant code

[<Measure>]
type usd

type Entree = 
    { Name : string
    ; Price : float<usd>
    ; Calories : int }

let entreeList =
    [ { Name = "Cheapest"; Price = 0.1<usd>; Calories = 300 }
    ; { Name = "Healthiest"; Price = 0.2<usd>; Calories = 100 } ]

let pickCheapest menuItems =
    List.reduce
        (fun acc item ->
            if item.Price < acc.Price
                then item
                else acc)
        menuItems

pickCheapest entreeList;;
// "Cheapest"

let pickHealthiest menuItems =
    List.reduce
        (fun acc item ->
            if item.Calories < acc.Calories
                then item
                else acc)
        menuItems

pickHealthiest entreeList;;
// "Healthiest"

let pickItem cmp menuItems =
    let reduceFunc acc item =
        match cmp acc item with // holds true if acc < item
        | true -> acc
        | false -> item
    List.reduce reduceFunc menuItems

// codes in the book might be wrong
let pickCheapest2 = pickItem (fun acc item -> acc.Price < item.Price)
let pickHealthiest2 = pickItem (fun acc item -> acc.Calories < item.Calories )

pickCheapest2 entreeList;;
pickHealthiest2 entreeList;;

// closures

type Set =
    {
        Add     : int -> Set
        Exists  : int -> bool
    }

    static member Empty =
        let rec makeSet lst = // I think "lst" actually stores the internal list
            {
                Add     = (fun item -> makeSet (item::lst))
                Exists  = (fun item -> List.exists ((=) item) lst)
            }
        makeSet []

let s1 = Set.Empty
let s2 = s1.Add 1
let s3 = s2.Add 2

s1.Exists(2);;
s2.Exists(2);;
s3.Exists(2);;
// false, false, true

#quit;;
