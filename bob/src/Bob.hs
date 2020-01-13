module Bob (responseFor) where

import           Data.Char as C

responseFor :: String -> String
responseFor s
  | xs == "" = "Fine. Be that way!"
  | isQuestion xs && isAllCaps xs = "Calm down, I know what I'm doing!"
  | isQuestion xs = "Sure."
  | isAllCaps xs = "Whoa, chill out!"
  | otherwise = "Whatever."
  where xs = filter (not . isSpace) s

isAllCaps :: String -> Bool
isAllCaps xs = map C.toUpper xs == xs && any (`elem` ['A'..'Z']) xs

isQuestion :: String -> Bool
isQuestion xs = last xs == '?'
