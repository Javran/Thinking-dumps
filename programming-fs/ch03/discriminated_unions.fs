// note this file only works in interactive mode

type Suit =
    | Heart
    | Diamond
    | Spade
    | Club;;

let suits =
    [ Heart
    ; Diamond
    ; Spade
    ; Club
    ] ;;

// associate optional data

type PlayingCard =
    | Ace   of Suit
    | King  of Suit
    | Queen of Suit
    | Jack  of Suit
    | ValueCard of int * Suit // the last item is a tuple which is a tuple
    ;;

let deckOfCards =
    [
        // list comprehension to generate a deck of cards
        for suit in [ Spade; Club; Heart; Diamond ] do
            yield Ace(suit)
            for value in 2..10 do
                yield ValueCard(value, suit)
            yield Jack(suit)
            yield Queen(suit)
            yield King(suit)
    ];;
// all possible playing cards

// we can also declare discriminated unions in a line
// note there's no '|' between '=' and the first item
type Numer = Odd | Even

// recursive definition
type Statement =
    | Print of string
    | Sequence of Statement * Statement
    | IfStmt of Expression * Statement * Statement

and Expression =
    | Integer       of int
    | LessThan      of Expression * Expression
    | GreaterThan   of Expression * Expression

(*
    if (3>1)
        print "3 is greater than 1"
    else
        print "3 is not "
        print "greater than 1"
*)

let program =
    IfStmt(
        GreaterThan(
            Integer(3),Integer(1)),
        Print("3 is greater than 1"),
        Sequence(
            Print("3 is not "),
            Print("greater than 1")));;
// and cheers!

// representing a binary tree
type BinaryTree =
    | Node of int * BinaryTree * BinaryTree // val, left tree, right tree
    | Empty // leaves
    ;;

let rec printInOrder =
    function 
    | Node (data, left, right) ->
        printInOrder left
        printfn "Node: %d" data
        printInOrder right
    | Empty -> ();;

printInOrder <|
    Node(2,
        Node(1, Empty, Empty),
        Node(4,
            Node(3, Empty, Empty),
            Node(5, Empty, Empty)
        )
    );;
(*
    2
   / \
   1 4
    / \
    3 5
*)

// pattern mathing against discriminated unions

let describeHoleCards =
    function
    | [] | [_]
        -> failwith "Too few cards."
    | cards when List.length cards > 2
        -> failwith "Too many cards."

    | [ Ace(_); Ace(_) ]
        -> "Pocket Rockets"
    | [ King(_); King(_) ]
        -> "Cowboys"

    | [ ValueCard(2, _); ValueCard(2, _) ]
        -> "Ducks"

    | [ Queen(_); Queen(_) ] | [ Jack(_); Jack(_) ]
        -> "Pair of face cards"
    | [ ValueCard(x, _); ValueCard(y, _) ] when x = y
        -> "A pair"

    | [ first; second ]
        -> sprintf "Two cards: %A and %A" first second

    | _
        -> failwith "impossible";; // impossible case to suppress warning

let testDescribeHoleCards =
    let testInList = [
        for x in [Ace(Diamond); King(Spade); Queen(Club); ValueCard(5, Heart)] do
            for y in deckOfCards do
                yield [x;y]
    ]

    List.iter (printfn "%s" << describeHoleCards) testInList;;

type Employee =
    | Manager of string * Employee list // list of Employee
    | Worker of string

let rec printOrganization =
    function
    | Worker(name) ->
        printfn "Employee %s" name
    | Manager(managerName, [ Worker(employeeName) ] ) ->
        printfn "Manager %s with Worker %s" managerName employeeName
    | Manager(managerName, [ Worker(employee1); Worker(employee2) ] ) ->
        printfn "Manager %s with two workers %s and %s"
            managerName employee1 employee2
    | Manager(managerName, workers) ->
        printfn "Manager %s with workers ..." managerName
        List.iter printOrganization workers
        
let company =
    Manager( "Tom", 
        [ Worker("Pam")
        ; Worker("Stuart")
        ; Manager( "Candy", [ Worker("Foo") ; Worker("Bar") ]) ]);;

printOrganization company;;

// add properties
type PlayingCardWithProperty =
    | Ace   of Suit
    | King  of Suit
    | Queen of Suit
    | Jack  of Suit
    | ValueCard of int * Suit

    member this.Value =
        function
        | Ace(_) -> 11
        | King(_) | Queen(_) | Jack(_) -> 10
        | ValueCard(x,_) when x <= 10 && x >= 2 -> x
        | _ -> failwith "Card has an invalid value."

let highCard = Ace(Spade)
let highCardValue = highCard.Value;;
printfn "%d" <| highCardValue highCard
// does not seem to be reasonable enough,
// for Value could be a static value...

#quit;;
