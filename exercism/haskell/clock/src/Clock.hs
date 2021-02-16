module Clock
  ( addDelta
  , fromHourMin
  , toString
  , Clock
  )
where

import Text.Printf

-- INVARIANT: 0 <= Clock < 24 * 60
newtype Clock = Clock Int deriving (Eq, Show)

fromHourMin :: Int -> Int -> Clock
fromHourMin hh mm = Clock $ (hhNorm + mmNorm) `mod` minsInDay
  where
    mmNorm = mm `mod` minsInDay
    hhNorm = (hh `mod` 24) * 60
    minsInDay = 24 * 60

addDelta :: Int -> Int -> Clock -> Clock
addDelta hh mm (Clock c) = fromHourMin hh (mm + c)

toString :: Clock -> String
toString (Clock c) = printf "%02d:%02d" hh mm
  where
    (hh, mm) = c `quotRem` 60
