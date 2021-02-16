module Clock
  ( fromHourMin
  , toString
  ) where

import Text.Printf

-- INVARIANT: 0 <= Int < 24 * 60
newtype Clock = Clock Int deriving (Eq,Show)

instance Num Clock where
    abs = undefined
    signum = undefined
    (*) = undefined

    fromInteger = mkClock . fromIntegral
    (Clock a) + (Clock b) = mkClock (a+b)
    negate (Clock v) = mkClock (24*60 - v)

mkClock :: Int -> Clock
mkClock = Clock . (`mod` (24 * 60))

fromHourMin :: Int -> Int -> Clock
fromHourMin hh mm = mkClock (hh*60 + mm)

toString :: Clock -> String
toString (Clock ms) = printf "%02d:%02d" hh mm
  where
    (hh, mm) = ms `quotRem` 60
