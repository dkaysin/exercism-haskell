module Meetup (Weekday(..), Schedule(..), meetupDay) where

import           Data.Time.Calendar
import           Data.Time.Calendar.OrdinalDate

data Weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday
             deriving (Eq, Enum, Show)

data Schedule = First
              | Second
              | Third
              | Fourth
              | Last
              | Teenth

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay schedule weekday year month = case schedule of
  First  -> head fdays
  Second -> fdays !! 1
  Third  -> fdays !! 2
  Fourth -> fdays !! 3
  Teenth -> head [d | d <- fdays
    , getDay d `elem `[13..19]]
  Last   -> last fdays
  where
    days = [firstDayOfMonth year month .. lastDayOfMonth year month]
    fdays = [d | d <- days
      , fromEnum weekday +1 == dayOfweek d]

firstDayOfMonth :: Integer -> Int -> Day
firstDayOfMonth year month = fromGregorian year month 1

lastDayOfMonth :: Integer -> Int -> Day
lastDayOfMonth year month = fromGregorian year month $ gregorianMonthLength year month

getDay :: Day -> Int
getDay day = dayN
  where (_, _, dayN) = toGregorian day

dayOfweek :: Day -> Int
dayOfweek day = weekday
  where (_, weekday) = mondayStartWeek day
