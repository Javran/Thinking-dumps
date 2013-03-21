module Utilities

module ConversionUtils =
    let intToString (x:int) = x.ToString()

    module ConvertBase =
        let convertToHex x = sprintf "%x" x

        let convertToOct x = sprintf "%o" x

module DataTypes =

    type Point = Point of float * float * float
