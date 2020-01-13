module LeapYear (isLeapYear) where

divides :: Integer -> Integer -> Bool
divides n m = n `mod` m == 0

isLeapYear :: Integer -> Bool
isLeapYear year =
  if divides year 4 then
    if divides year 100 then
      divides year 400
    else True
  else False
