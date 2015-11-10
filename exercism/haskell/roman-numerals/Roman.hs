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
numerals n =
       thousands !! a
    <> hundreds !! b
    <> tens !! c
    <> ones !! d
  where
    (n1, d) = n  `quotRem` 10
    (n2, c) = n1 `quotRem` 10
    (n3, b) = n2 `quotRem` 10
    (_,  a) = n3 `quotRem` 100

    mkTable i v x =
        [ mempty
        , i, i<>i, i<>i<>i, i<>v
        , v, v<>i, v<>i<>i, v<>i<>i<>i
        , i<>x ]
    ones = mkTable "I" "V" "X"
    tens = mkTable "X" "L" "C"
    hundreds = mkTable "C" "D" "M"
    thousands = mkTable "M" mempty mempty
