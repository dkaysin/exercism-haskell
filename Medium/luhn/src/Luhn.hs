module Luhn (isValid) where

import           Data.Char

isValid :: String -> Bool
isValid xs
  | any isAlpha xs = False
  | length sanitized <= 1 = False
  | otherwise = calc `mod` 10 == 0
  where
    sanitized = filter isDigit xs
    calc =
      sum
      $ zipWith mult (concat (repeat [1,2]))
      $ reverse $ map digitToInt sanitized
    mult a b
      | p <= 9 = p
      | otherwise = p - 9
      where p = a * b
