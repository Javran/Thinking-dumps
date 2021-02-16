module Atbash
  ( encode
  ) where

import Data.Char
import Data.List.Split

-- | encode normalized characters
--   the input character must be one possible element of
--   @normalizeInput xs@
encodeNChar :: Char -> Char
encodeNChar x
    | isDigit x = x
encodeNChar x = chr (ord 'a' + j)
  where
    i = ord x - ord 'a'
    j = 25 - i

-- | filter input leaving only 0-9A-Za-z
--   then cast letters into lower cases
normalizeInput :: String -> String
normalizeInput = map toLower . filter isValid
  where
    -- from testcases, we guess the only valid inputs
    -- are A-Za-z0-9
    isValid x = isDigit x
             || isAsciiLower x
             || isAsciiUpper x

formatOutput :: String -> String
formatOutput = unwords . chunksOf 5

encode :: String -> String
encode = formatOutput
       . map encodeNChar
       . normalizeInput
