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
isPangram = notNull . dropWhile notNull . scanl go alphabet
  where
    notNull = not . null
    go missingChars (toUpper -> ch) =
      S.delete ch missingChars
