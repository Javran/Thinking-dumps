module ArmstrongNumbers
  ( armstrong
  )
where

import Data.List

armstrong :: Integral a => a -> Bool
armstrong 0 = True
armstrong x =
  {-
    the value is computed in Integer because the result of exponentiation
    might overflow in the original type.
   -}
  toInteger x == sum (fmap ((^ l) . toInteger) ds)
  where
    l = length ds
    ds = intToDigitsRev x

intToDigitsRev :: Integral i => i -> [i]
intToDigitsRev = unfoldr f
  where
    f 0 = Nothing
    f n = let (q, r) = n `quotRem` 10 in Just (r, q)
