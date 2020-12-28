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

-- | plaintexts can contain only 0-9A-Za-z
--   other characters are filtered out.
--   additionally, all plaintexts should be converted to lowercases
normalizePlaintext :: String -> String
normalizePlaintext = map toLower . filter isValid
  where
    isValid x = isDigit x
             || isAsciiUpper x
             || isAsciiLower x

-- | given normalized plaintext, calculate numbers of columns for it
squareSize :: [a] -> Int
squareSize xs = ceiling (sqrt (fromIntegral l) :: Double)
  where
    l = length xs

-- | normalize plaintext and then break it into rectangle
plaintextSegments :: String -> [String]
plaintextSegments raw = chunksOf col xs
  where
    xs = normalizePlaintext raw
    col = squareSize xs

normalizeCiphertext :: String -> String
normalizeCiphertext = unwords . transpose . plaintextSegments

ciphertext :: String -> String
ciphertext = concat . transpose . plaintextSegments
