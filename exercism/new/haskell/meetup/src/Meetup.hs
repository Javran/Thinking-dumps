module Meetup
    ( Weekday(..)
    , Schedule(..)
    , meetupDay
    )
where

import Control.Lens
import Data.Time.Calendar
import Data.Time.Calendar.MonthDay
import Data.Time.Calendar.WeekDate
import Data.Ix

data Weekday
    = Sunday     -- 0
    | Monday     -- 1
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday   -- 6
      deriving (Enum, Eq)

data Schedule
    = Teenth
    | First
    | Second
    | Third
    | Fourth
    | Last

meetupDay :: Schedule -> Weekday -> Int -> Int -> Day
meetupDay sche week year month = fst $ case sche of
    Teenth -> head . filter isTeenth $ days
    First  -> head days
    Second -> days !! 1
    Third  -> days !! 2
    Fourth -> days !! 3
    Last   -> last days
    where
        mLen = monthLength (isLeapYear . fromIntegral $ year) month
        allDays :: [(Day, Weekday)]
        -- e.g. [(1th,Tuesday), (2nd,Wednesday) ...]
        allDays = take mLen
                . map ( dayOfWeekPair
                      . fromGregorian
                            (fromIntegral year)
                            month)
                $ [1..]
        days = filter ((== week) . snd) allDays -- only valid days
        dayOfWeekPair :: Day -> (Day, Weekday)
        dayOfWeekPair x = (\(_,_,w) -> (x, toEnum . (`mod` 7) $ w))
                        . toWeekDate $ x
        isTeenth :: (Day, a) -> Bool
        isTeenth = inRange (13,19) . view _3 . toGregorian . fst
