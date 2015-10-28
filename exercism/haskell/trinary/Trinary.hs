module Trinary
  ( showTri
  , readTri
  ) where

import Data.List
import Data.Maybe
import Data.Char

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

showTri :: Integral a => a -> String
showTri v = map (toDigit . fromIntegral . snd) $ reverse $ tail $ takeWhile (\(x,y) -> x /= 0 || y /= 0) $  iterate (\(x,_) -> x `quotRem` 3) (v,0)
  where
    toDigit i = chr (ord '0' + i)
{-

import Data.Maybe
import Data.List
-}
