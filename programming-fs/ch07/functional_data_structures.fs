// note this file only works in interactive mode

open System
open System.IO
open System.Net

let getHtml (url : string) =
    let req = WebRequest.Create(url)
    let rsp = req.GetResponse()

    use stream = rsp.GetResponseStream()
    use reader = new StreamReader( stream )

    reader.ReadToEnd()

let uniqueWords (text : string) =
    let words = text.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)

    let uniqueWords =
        Array.fold
            (fun (acc : Set<string>) (word : string) ->
                Set.add word acc)
            Set.empty
            words
    uniqueWords

let frankensteinBookUrl = @"http://www.gutenberg.org/files/84/84.txt"

let wordsInBook bookUrl =
    bookUrl
    |> getHtml
    |> uniqueWords;;

// uncomment the line below to see the result
//Set.count (wordsInBook frankensteinBookUrl)

let wordUsage (text : string) =
    let words = text.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)

    let wordFrequency =
        Array.fold
            (fun (acc : Map<string, int>) (word : string) ->
                if Map.containsKey word acc
                    then Map.add word (acc.[word] + 1) acc
                    else Map.add word 1 acc)
            Map.empty
            words
    wordFrequency

let printMostFrequentWords (wordFrequency : Map<string, int>) =
    let top20Words =
        wordFrequency
        |> Map.toSeq
        |> Seq.sortBy (fun (word, timesUsed) -> -timesUsed)
        |> Seq.take 20

    printfn "Top Word Usage:"
    top20Words
    |> Seq.iteri (fun idx (word, timesUsed) ->
                    printfn "%d\t '%s' was used %d times" idx word timesUsed);;

// uncomment the following block to see the result
(*
frankensteinBookUrl
|> getHtml
|> wordUsage
|> printMostFrequentWords;;
*)

#quit;;
