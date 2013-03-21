#load "NestedModules.fs"

let num = 65535

printfn "intToString: %s" (Utilities.ConversionUtils.intToString num)

printfn "hex: %s" (Utilities.ConversionUtils.ConvertBase.convertToHex num)
// ffff
printfn "oct: %s" (Utilities.ConversionUtils.ConvertBase.convertToOct num)
// 177777 

#quit;;
