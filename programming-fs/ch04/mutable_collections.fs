// note this file only works in interactive mode

open System.Collections.Generic

let planets = new List<string>();;

[ "Mercury"; "Venus"; "Earth"; "Mars" ]
    |> Seq.iter planets.Add;;

planets.Count;;
// 4

planets.AddRange( [| "Jupiter"
                   ; "Saturn"
                   ; "Uranus"
                   ; "Neptune"
                   ; "Pluto" |] );;

planets.Count;;
// 9

planets.Remove "Pluto";;
// true (found and removed)

planets.Count;;
// 8

// actually F# and C# are using the same class here
// (note this one is not the one used by F#, which is immutable)

// Atomic Mass Units
[<Measure>]
type amu

type Atom =
    { Name : string
    ; Weight : float<amu> }

let periodicTable = new Dictionary<string, Atom>()

[ ( "H",  { Name = "Hydrogen";  Weight = 1.0079<amu> } ) 
; ( "He", { Name = "Helium";    Weight = 4.0026<amu> } ) 
; ( "Li", { Name = "Lithitm";   Weight = 6.9410<amu> } )
; ( "Be", { Name = "Beryllium"; Weight = 9.0122<amu> } )
; ( "B" , { Name = "Boron";     Weight = 10.811<amu> } )]
    |> Seq.iter (fun (k,v) -> periodicTable.Add(k,v));;

periodicTable;;

// Lookup an element
let printElement name =
    if periodicTable.ContainsKey(name)
        then
            let atom = periodicTable.[name]
            printfn
                "Atom with symbol with '%s' has weight %A."
                atom.Name atom.Weight
        else
            printfn
                "Error. No atom with name '%s' found." name

printElement "H"
printElement "B"
printElement "Null"

let printElement2 name =
    let (found,atom) = periodicTable.TryGetValue(name)
    if found
        then
            printfn
                "Atom with symbol with '%s' has weight %A."
                atom.Name atom.Weight
        else
            printfn
                "Error. No atom with name '%s' found." name

printElement2 "He"
printElement2 "Li"
printElement2 "X"

let bestPicture = new HashSet<string>()

[ "The Artist"
; "The King's Speech"
; "The Hurt Locker"
; "Slumdog Milionaire"
; "No Country for Old Men"
; "The Departed" ]
    |> Seq.iter (bestPicture.Add >> ignore)

if bestPicture.Contains("Manos: The Hands of Fate")
    then printfn "Sweet..."

Seq.iter (printfn "%A") bestPicture

#quit;;
