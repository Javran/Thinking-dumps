// note this file only works in interactive mode

open System.IO

// single case active patterns
let determineFileType (filePath:string) =
    // note here argument "filePath" is not the one passed into the body of "determineFileType"
    let (| FileExtension |) filePath = Path.GetExtension(filePath)
    match filePath with
    // without active patterns
    | filePath when Path.GetExtension(filePath) = ".txt" ->
        printfn "It is a text file."

    // converting the data using an active pattern
    | FileExtension ".jpg"
    | FileExtension ".png"
    | FileExtension ".gif" ->
        printfn "It is an image file."

    // binding a new value
    | FileExtension ext ->
        printfn "Unknown file extension [%s]" ext

[ "aaa.txt"; "aaa.jpg"; "aaa.png"; "aaa.dat" ]
    |> Seq.iter determineFileType

// some pattern might fail ... we need to resort on "option"
//     -- *partial active pattern* is the solution...


open System

// TODO: if we can parameterized Boolean.TryParse / Double.TryParse ...
//     we will have a more general active pattern
//     but I don't know if it can be achieved in F# ...
let (|ToBool|_|) x =
    let success, result = Boolean.TryParse(x)
    if success
        then Some(result)
        else None

let (|ToInt|_|) x =
    let success, result = Int32.TryParse(x)
    if success
        then Some(result)
        else None

let (|ToFloat|_|) x =
    let success, result = Double.TryParse(x)
    if success
        then Some(result)
        else None

let describeStringTest =
    let describeString str =
        match str with
        | ToBool b ->
            printfn "%s is a bool with value %b" str b
        | ToInt i ->
            printfn "%s is an int with value %d" str i
        | ToFloat f ->
            printfn "%s is a float with value %f" str f
        | _ ->
            printfn "%s is not a bool, int or float" str

    [ "true"; "12345"; "123456.789"; "Something else" ] 
        |> Seq.iter describeString

open System.Text.RegularExpressions

let (|RegexMatch3|_|) (pattern : string) (input : string) =
    let result = Regex.Match(input, pattern)

    if result.Success
        then
            match (List.tail [ for g in result.Groups -> g.Value ]) with
            | fst :: snd :: trd :: [] ->
                Some(fst, snd, trd)
            | [] ->
                failwith "Match succeeded, but no groups found."
            | _ ->
                failwith "Match succeeded, but did not find exactly 3 groups"
        else
            None

let parseTime input =
    match input with
    | RegexMatch3 "(\d+)/(\d+)/(\d\d\d\d)" (month, day, year)
    // "or"
    | RegexMatch3 "(\d\d\d\d)-(\d+)-(\d+)" (year, month, day) ->
        Some( new DateTime(int year, int month, int day) )
    | _ -> None
    
let testParseTime =
    [ "2013-4-30"; "04/30/2013"; "invalid" ]
        |> Seq.iter (printfn "%A" << parseTime)


let (|Paragraph|Sentence|Word|WhiteSpace|) (rawInput : string) =
    let input = rawInput.Trim()
    
    match input with
    | _ when input = "" ->
        WhiteSpace
    | _ when input.IndexOf(".") <> -1 ->
        Paragraph(input.Split('.'))
    | _ when input.IndexOf(" ") <> -1 ->
        Sentence(input.Split(' '))
    | _ -> Word(input)

let rec countLetters =
    function
    | WhiteSpace -> 0
    | Word x -> x.Length
    | Sentence words ->
        words
            |> Array.map countLetters
            |> Array.sum
    | Paragraph sentences ->
        sentences
            |> Array.map countLetters
            |> Array.sum

let testCountLetters =
    let testCase = "This active pattern divides all strings into their various meanings."
    let countResult = countLetters testCase
    let actualResult =
        testCase
        |> Seq.filter System.Char.IsLetter
        |> Seq.length

    printfn "%d,%d" countResult actualResult
    // 58,58

// active pattern can be used anywhere that pattern matching can be used

let testActivePattern =
    let (|ToUpper|) (input:string) = input.ToUpper()
    let f (ToUpper x) = printfn "x = %s" x
    
    f "this is lower case";;

// active patterns can be combined by using "&"
let (|KBInSize|MBInSize|GBInSize|) (_,size) =
    let block = 1024L
    if size < block * block
        then KBInSize
    else if size < block * block * block
        then MBInSize
        else GBInSize

let (|IsImageFile|_|) (fileName,size) =
    let (|EndsWithExtension|) (input:string,_) =
        input.Substring(input.LastIndexOf('.'))

    match (fileName,size) with
    | EndsWithExtension ".jpg"
    | EndsWithExtension ".bmp"
    | EndsWithExtension ".gif" ->
        Some()
    | _ ->
        None

let ImageTooBigForEmail =
    function
    | IsImageFile & (MBInSize | GBInSize) -> true
    | _ -> false

[ "aa.jpg", 10000L 
; "bb.gif", 100000000L
; "cc.txt", 1L]
    |> Seq.map ImageTooBigForEmail
    |> Seq.iter (printfn "%b")
// false,true,false

// nesting active patterns

#r "System.Xml.dll"
open System.Xml

let (|Elem|_|) name (inp: XmlNode) =
    if inp.Name = name
        then Some(inp)
        else None

let (|Attributes|) (inp: XmlNode) =
    inp.Attributes

let (|Attr|) attrName (inp: XmlAttributeCollection) =
    match inp.GetNamedItem(attrName) with
    | null -> failwithf "Attribute %s not found" attrName
    | attr -> attr.Value

type Part =
    | Widget    of float
    | Sprocket  of string * int

let ParseXmlNode element =
    match element with
    | Elem "Widget" xmlElement ->
        match xmlElement with
        | Attributes xmlElementsAttributes ->
            match xmlElementsAttributes with
            | Attr "Diameter" diameter ->
                Widget(float diameter)
    | Elem "Sprocket" (Attributes (Attr "Model" model & Attr "SerialNumber" sn)) ->
        Sprocket(model, int sn)
    | _ -> failwith "Unknown element"

let testXmlParse =
    let xmlDoc =
        let doc = new System.Xml.XmlDocument()
        let xmlText = 
            "<?xml version=\"1.0\" encoding=\"utf-8\"?>
                <Parts>
                    <Widget Diameter='5.0' />
                    <Sprocket Model='A' SerialNumber='147' />
                    <Sprocket Model='B' SerialNumber='302' />
                </Parts>"
        doc.LoadXml(xmlText)
        doc
    xmlDoc.DocumentElement.ChildNodes
    |> Seq.cast<XmlElement>
    |> Seq.map ParseXmlNode
    |> Seq.iter (printfn "%A")
    // Widget 5.0
    // Sprocket 'A' 147
    // Sprocket 'B' 302

#quit;;
