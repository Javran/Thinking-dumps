module Luhn
  ( checkDigit
  , addends
  , checksum
  , isValid
  , create)
  where

checkDigit :: Integer -> Integer
checkDigit = (`rem` 10)

toDigits :: Integer -> [Integer]
toDigits 0 = [0]
toDigits v =
      map snd
    . reverse
    . takeWhile (\(x,y) -> x > 0 || y > 0)
    . tail
    -- [(123,0),(12,3),(1,2),(0,1),(0,0]
    $ iterate (\(x,_) -> x `quotRem` 10) (v,0)
    -- e.g. v=123

addends :: Integer -> [Integer]
addends = undefined

checksum :: Integer -> Integer
checksum = undefined

isValid :: Integer -> Bool
isValid = undefined

create :: Integer -> Integer
create = undefined
