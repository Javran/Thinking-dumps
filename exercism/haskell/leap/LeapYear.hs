module LeapYear
    ( isLeapYear
    )
where

-- | true if the given year is leap year
isLeapYear :: Int -> Bool
isLeapYear n
    | not (n `divisibleBy` 4) = False
    -- every cond below satisfies "n `divisibleBy` 4"
    | n `divisibleBy` 400     = True
    | otherwise               = not (n `divisibleBy` 100)
    where
        x `divisibleBy` y = x `mod` y == 0
