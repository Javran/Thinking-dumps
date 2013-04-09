// note this file only works in interactive mode

type Info = { State : string; ZipCode : string };;

let allCustomers = [ { State = "State1"; ZipCode = "123456" }
                   ; { State = "State1"; ZipCode = "654321" }
                   ; { State = "State2"; ZipCode = "654333" }
                   ; { State = "State2"; ZipCode = "654333" } ];;

let customerZipCodesByState stateName =
    allCustomers
        |> Seq.filter (fun customer -> customer.State = stateName)
        |> Seq.map (fun customer -> customer.ZipCode)
        |> Seq.distinct;;
    
customerZipCodesByState "State1"
    |> Seq.iter (printfn "%s");;
// 2 zip codes printed

customerZipCodesByState "State2"
    |> Seq.iter (printfn "%s");;
// 1 zip code printed

// "query" enables us to use SQL-like syntax
let customerZipCodesByStateQuery stateName =
    query {
        for customer in allCustomers do
            where (customer.State = stateName)
            select customer.ZipCode
            distinct
    };;

customerZipCodesByStateQuery "State2"
    |> Seq.iter (printfn "%s");;
// 1 zip code printed

#quit;;
