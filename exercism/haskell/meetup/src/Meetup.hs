module Meetup
  ( Weekday (..)
  , Schedule (..)
  , meetupDay
  )
where

import Control.Monad
import Data.Ix
import Data.Maybe
import Data.Time.Calendar
import Data.Time.Calendar.MonthDay
import Data.Time.Calendar.WeekDate

data Weekday
  = Sunday -- 0
  | Monday -- 1
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday -- 6
  deriving (Enum, Eq)

data Schedule
  = Teenth
  | First
  | Second
  | Third
  | Fourth
  | Last

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay sche week year month = fst $ case sche of
  Teenth -> head . filter isTeenth $ days
  First -> head days
  Second -> days !! 1
  Third -> days !! 2
  Fourth -> days !! 3
  Last -> last days
  where
    days :: [(Day, Weekday)]
    days = mapMaybe (\p@(_, w) -> p <$ guard (w == week)) allDays -- only valid days
      where
        -- e.g. [(1st,Tuesday), (2nd,Wednesday) ...]
        allDays = map (dayOfWeekPair . fromGregorian year month) [1 .. mLen]
        mLen = monthLength (isLeapYear year) month
    dayOfWeekPair :: Day -> (Day, Weekday)
    dayOfWeekPair x = (\(_, _, w) -> (x, toEnum (w `rem` 7))) . toWeekDate $ x
    isTeenth :: (Day, a) -> Bool
    isTeenth = inRange (13, 19) . (\(_, _, x) -> x) . toGregorian . fst
