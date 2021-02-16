module Anagram
  ( anagramsFor
  )
where

import Data.Char
import Data.List

anagramsFor :: String -> [String] -> [String]
anagramsFor x = filter isCandidate
  where
    {-
      Normalize a word so that words have same bag of characters have
      the same projected value. In this case the projected value
      is the lowercased and sorted word.
     -}
    xLow = fmap toLower x
    x' = sort xLow

    isCandidate y = y' == x' && yLow /= xLow
      where
        yLow = fmap toLower y
        y' = sort yLow
