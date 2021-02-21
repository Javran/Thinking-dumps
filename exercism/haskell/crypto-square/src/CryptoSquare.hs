{-# LANGUAGE TypeApplications #-}

module CryptoSquare
  ( normalizePlaintext
  , squareSize
  , plaintextSegments
  , encode
  )
where

import Data.Char
import Data.List
import Data.List.Split

-- | plaintexts can contain only 0-9A-Za-z
--   other characters are filtered out.
--   additionally, all plaintexts should be converted to lowercases
normalizePlaintext :: String -> String
normalizePlaintext = map toLower . filter isValid
  where
    isValid x =
      isDigit x
        || isAsciiUpper x
        || isAsciiLower x

-- | given normalized plaintext, calculate numbers of columns for it
squareSize :: [a] -> Int
squareSize xs = ceiling (sqrt @Double (fromIntegral l))
  where
    l = length xs

-- | normalize plaintext and then break it into rectangle
plaintextSegments :: String -> [String]
plaintextSegments raw = chunksOf col xs
  where
    xs = normalizePlaintext raw
    col = squareSize xs

encode :: String -> String
encode = unwords . transpose . plaintextSegments
