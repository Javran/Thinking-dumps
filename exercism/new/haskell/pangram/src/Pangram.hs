{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Pangram
  ( isPangram
  )
where

import Data.Char
import qualified Data.Set as S

alphabet :: S.Set Char
alphabet = S.fromDistinctAscList ['A' .. 'Z']

isPangram :: String -> Bool
isPangram xs = foldr go (const False) xs alphabet
  where
    go (toUpper -> ch) acc (S.delete ch -> missingChars) =
      null missingChars || acc missingChars
