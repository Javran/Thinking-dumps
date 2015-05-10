{-# LANGUAGE TupleSections #-}
module Scrabble
    ( scoreLetter
    , scoreWord
    )
where

import Data.Char
import qualified Data.Array.Unboxed as UA

scoreLetter :: Char -> Int
scoreLetter = value . toLower
  where
    value x =
      if UA.inRange scoreBnd x
        then scoreArr UA.! x
        else 0

scoreBnd :: (Char,Char)
scoreBnd = ('a','z')

scoreArr :: UA.UArray Char Int
scoreArr = UA.array scoreBnd funcDef
  where
    inp ~= val = map (,val) inp
    funcDef = concat
      [ "aeioulnrst" ~= 1
      , "dg"         ~= 2
      , "bcmp"       ~= 3
      , "fhvwy"      ~= 4
      , "k"          ~= 5
      , "jx"         ~= 8
      , "qz"         ~= 10
      ]

scoreWord :: String -> Int
scoreWord = sum . map scoreLetter
