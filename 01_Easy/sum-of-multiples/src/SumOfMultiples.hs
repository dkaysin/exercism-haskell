module SumOfMultiples (sumOfMultiples) where

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum $ filter (checkFactors factors) [1..limit-1]

checkFactors :: [Integer] -> Integer -> Bool
checkFactors factors n = any (n `divides`) $ filter (/= 0) factors

divides :: Integer -> Integer -> Bool
divides n k
  | n `mod` k == 0 = True
  | otherwise = False
