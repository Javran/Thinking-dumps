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

#quit;;
