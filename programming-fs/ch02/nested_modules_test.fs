#load "NestedModules.fs"

let num = 65535

printfn "intToString: %s" (Utilities.ConversionUtils.intToString num)

printfn "hex: %s" (Utilities.ConversionUtils.ConvertBase.convertToHex num)
// ffff
printfn "oct: %s" (Utilities.ConversionUtils.ConvertBase.convertToOct num)
// 177777 

// I'm confusing about why should F# have namespace and modules at the same time
//     skiping tests on namespace, let's just follow what's been said in book:
//     "just put everything in a module"

#quit;;
