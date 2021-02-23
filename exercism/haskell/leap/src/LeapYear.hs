module LeapYear
  ( isLeapYear
  )
where

-- | Tests whether a given year is a leap year.
isLeapYear :: Int -> Bool
isLeapYear n =
  (n `rem` 4 == 0)
    && ((n `rem` 100 /= 0) || (n `rem` 400 == 0))
