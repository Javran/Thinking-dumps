// note this file only works in interactive mode

let x = Lazy<int>.Create( fun () -> printfn "Evaluatiing x ..."; 10 );;
let y = lazy (printfn "Evaluatiing y ..."; x.Value + x.Value);;

// note the output is "Value is not created"
x;;
// "Value is not created" as well

// we force things to be eval-ed by ".Value"
y.Value;;
// 20, and 2 "Evaluatiing ..." hints
x.Value;;
// 10, no more because x has already been evaluated

// the value after evaluation is stored and guaranteed to be immutable,
//     namely, no side effect

#quit;;
