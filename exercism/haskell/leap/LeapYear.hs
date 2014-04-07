module LeapYear
    ( isLeapYear
    )
where

-- | true if the given year is leap year
isLeapYear :: Int -> Bool
isLeapYear n
    | not (n `divisibleBy` 4) = False
    | n `divisibleBy` 400     = True
    | n `divisibleBy` 4       = not (n `divisibleBy` 100)
    where
        x `divisibleBy` y = x `mod` y == 0
