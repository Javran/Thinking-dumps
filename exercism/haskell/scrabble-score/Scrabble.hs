module Scrabble
    ( scoreLetter
    , scoreWord
    )
where

import Data.Char (toLower)

scoreLetter :: Char -> Int
scoreLetter = value . toLower
    where
        value x
            | oneOf "aeioulnrst" = 1
            | oneOf "dg"         = 2
            | oneOf "bcmp"       = 3
            | oneOf "fhvwy"      = 4
            | oneOf "k"          = 5
            | oneOf "jx"         = 8
            | oneOf "qz"         = 10
            | otherwise          = undefined
            where oneOf = (x `elem`)

scoreWord :: String -> Int
scoreWord = sum . map scoreLetter
