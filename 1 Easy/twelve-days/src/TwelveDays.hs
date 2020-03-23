module TwelveDays (recite) where

recite :: Int -> Int -> [String]
recite start stop = map line [start-1 .. stop-1]

line :: Int -> String
line n =
  "On the " ++ (numbers !! n) ++ " day of Christmas "
  ++ "my true love gave to me: "
  ++ (joinStrings . getItems) (reverse [0 .. n]) ++ "."

joinStrings :: [String] -> String
joinStrings xs = concatMap (\(s, delim) -> s ++ delim) $ zip xs $ generateDelims $ length xs

generateDelims :: Int -> [String]
generateDelims n
  | n >= 2 = replicate (n-2) ", " ++ [", and "] ++ [""]
  | n >= 0 = replicate n ""
  | otherwise = []

getItems :: [Int] -> [String]
getItems = map (items !!)

numbers :: [String]
numbers = [
      "first"
    , "second"
    , "third"
    , "fourth"
    , "fifth"
    , "sixth"
    , "seventh"
    , "eighth"
    , "ninth"
    , "tenth"
    , "eleventh"
    , "twelfth"
  ]

items :: [String]
items = [
      "a Partridge in a Pear Tree"
    , "two Turtle Doves"
    , "three French Hens"
    , "four Calling Birds"
    , "five Gold Rings"
    , "six Geese-a-Laying"
    , "seven Swans-a-Swimming"
    , "eight Maids-a-Milking"
    , "nine Ladies Dancing"
    , "ten Lords-a-Leaping"
    , "eleven Pipers Piping"
    , "twelve Drummers Drumming"
  ]
