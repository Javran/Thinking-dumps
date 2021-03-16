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

{-
  The following is based on the idea of "bouncy-folds", which IMO isn't
  very straightforward but more concise:

  https://github.com/quchen/articles/blob/master/useful_techniques.md#bouncy-folds
 -}

_isPangram :: String -> Bool
_isPangram xs = foldr go (const False) xs alphabet
  where
    go (toUpper -> ch) acc (S.delete ch -> missingChars) =
      null missingChars || acc missingChars
