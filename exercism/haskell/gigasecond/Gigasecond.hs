module Gigasecond
    ( fromDay )
where

import Data.Time.Calendar

fromDay :: Day -> Day
fromDay = addDays days
    where
        oneGs :: Double
        oneGs = 10 ** 9 -- seconds
        days = floor (oneGs / 3600 / 24)
