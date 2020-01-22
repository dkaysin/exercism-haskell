module Triangle (rows) where

import           Data.List

rows :: Int -> [[Integer]]
rows n = take n $ iterate next [1]

next :: [Integer] -> [Integer]
next xn = map sum $ filter ((==2).length) $ map (take 2) $ tails padded
  where padded = [0] ++ xn ++ [0]
