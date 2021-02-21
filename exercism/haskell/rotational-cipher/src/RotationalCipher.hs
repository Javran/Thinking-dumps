module RotationalCipher
  ( rotate
  )
where

import Data.Char

rotate :: Int -> String -> String
rotate offset = fmap translate
  where
    changeByBase chBase ch =
      chr $ ((ord ch - baseOrd + offset) `rem` 26) + baseOrd
      where
        baseOrd = ord chBase
    translate ch
      | isAsciiUpper ch = changeByBase 'A' ch
      | isAsciiLower ch = changeByBase 'a' ch
      | otherwise = ch
