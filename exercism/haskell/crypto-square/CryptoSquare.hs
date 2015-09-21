module CryptoSquare
  ( normalizePlaintext
  , squareSize
  , plaintextSegments
  , ciphertext
  , normalizeCiphertext
  ) where

import Data.Char
import Data.List.Split
import Data.List

normalizePlaintext :: String -> String
normalizePlaintext = map toLower . filter isValid
  where
    isValid x = isDigit x
             || isAsciiUpper x
             || isAsciiLower x

squareSize :: [a] -> Int
squareSize xs = ceiling (sqrt (fromIntegral l) :: Double)
  where
    l = length xs

plaintextSegments :: String -> [String]
plaintextSegments raw = chunksOf col xs
  where
    xs = normalizePlaintext raw
    col = squareSize xs

normalizeCiphertext :: String -> String
normalizeCiphertext = unwords . transpose . plaintextSegments

ciphertext :: String -> String
ciphertext = concat . transpose . plaintextSegments
