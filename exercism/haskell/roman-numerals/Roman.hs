module Roman
  ( numerals
  ) where

import Data.Monoid

{-
  we can break a number into multiple parts,
  convert them into roman-numerals separately
  and combine the result.

  for a number ABCD, A,B,C,D are independent of each other

-}

numerals :: Int -> String
numerals v = undefined
  where
    mkTable i v x =
        [ mempty
        , i, i <> i, i <> i <> i, i <> v
        , v, v <> i, v <> i <> i, v <> i <> i <> i
        , i <> x ]
    ones = mkTable "I" "V" "X"
    tens = mkTable "X" "L" "C"
    hundreds = mkTable "C" "D" "M"
    thousands = take (3+1) (mkTable "M" mempty mempty)
