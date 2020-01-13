module ArmstrongNumbers (armstrong) where

armstrong :: Integral a => a -> Bool
armstrong n = n == sum (map (^l) ds)
  where
    ds = getDigits n
    l = length ds

getDigits :: Integral a => a -> [a]
getDigits z
  | z == 0 = []
  | otherwise = (z `mod` 10) : getDigits (z `div` 10)
