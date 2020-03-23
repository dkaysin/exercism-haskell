module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n
  | n > 0 = case compare (sum $ getFactors n) n of
    EQ -> Just Perfect
    LT -> Just Deficient
    GT -> Just Abundant
  | otherwise = Nothing

getFactors :: Int -> [Int]
getFactors n = filter ((==0) . (n `mod`)) [1..n-1]
