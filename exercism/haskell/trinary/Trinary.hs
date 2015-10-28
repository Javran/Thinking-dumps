module Trinary
  ( showTri
  , readTri
  ) where
 
{-
       module Binary
  (toDecimal)
  where

import Data.Maybe
import Data.List

-- | converts a binary presentation (in String)
--   to integer, invalid presentation results in 0
toDecimal :: Integral a => String -> a
toDecimal = foldl' go 0 . parseBinary
  where
    -- accumulate value
    go acc i = acc*2 + fromIntegral i

-- | parses a character as 0/1
parseDigit :: Char -> Maybe Int
parseDigit '0' = Just 0
parseDigit '1' = Just 1
parseDigit _ = Nothing

-- | parses binary strings
parseBinary :: String -> [Int]
parseBinary = fromMaybe [] . mapM parseDigit
-}
