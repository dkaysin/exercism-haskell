module PrimeFactors (primeFactors) where

primeFactors :: Integer -> [Integer]
primeFactors n = temp n 2

temp :: Integer -> Integer -> [Integer]
temp n dv
  | n < dv = []
  | (isPrime dv) && (n `mod` dv == 0) = dv : temp (n `div` dv) dv
  | otherwise = temp n (dv+1)

isPrime :: Integer -> Bool
isPrime 1 = False
isPrime n = null [k | k <- [2..ubound], n `mod` k == 0]
  where ubound = floor (sqrt (fromIntegral n))
