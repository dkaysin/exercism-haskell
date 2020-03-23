module Prime (nth) where

import           Data.List

nth :: Int -> Maybe Integer
nth n
  | n <= 0 = Nothing
  | n == 1 = Just 2
  | otherwise = Just $ head $ foldl' (\acc _ -> nxt acc) [3, 2] [1..(n-2)]

nxt :: [Integer] -> [Integer]
nxt xn = found : xn
  where found = head $ filter (checkDivision xn) [(head xn+2), (head xn+4) ..]

checkDivision :: [Integer] -> Integer -> Bool
checkDivision xn k = all (\x -> k `mod` x /= 0) $ filter (<=mx) xn
  where mx = ceiling $ sqrt $ fromIntegral k
