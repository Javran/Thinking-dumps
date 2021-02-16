{-# LANGUAGE ScopedTypeVariables #-}
module Hexadecimal
  ( hexToInt
  ) where

import Data.Maybe
import Data.Char

hexToInt :: forall a. Integral a => String -> a
hexToInt xs = fromMaybe 0 $
                -- parse and accumulate digits
                compute <$> mapM toDigit xs
  where
    compute :: [Int] -> a
    compute = foldl (\acc i -> acc*16 + fromIntegral i) 0
    toDigit :: Char -> Maybe Int
    toDigit c
        | isDigit c = Just $ ord c - ord '0'
        | c `elem` "ABCDEF" = Just $ ord c - ord 'A' + 10
        | c `elem` "abcdef" = Just $ ord c - ord 'a' + 10
        | otherwise = Nothing
