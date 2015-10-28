module Trinary
  ( showTri
  , readTri
  ) where

import Data.List
import Data.Maybe

-- | converts a trinary presentation (in String)
--   to integer, invalid presentation results in 0
readTri :: Integral a => String -> a
readTri = foldl' go 0 . parseBinary
  where
    -- accumulate value
    go acc i = acc*3 + fromIntegral i

-- | parses a character as 0/1
parseDigit :: Char -> Maybe Int
parseDigit '0' = Just 0
parseDigit '1' = Just 1
parseDigit '2' = Just 2
parseDigit _ = Nothing

-- | parses binary strings
parseBinary :: String -> [Int]
parseBinary = fromMaybe [] . mapM parseDigit

showTri :: a
showTri = undefined

{-

import Data.Maybe
import Data.List
-}
