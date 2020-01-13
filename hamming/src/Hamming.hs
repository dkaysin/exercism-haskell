module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance xs ys
  | length xs /= length ys = Nothing
  | otherwise = Just $ foldl reducer 0 (zip xs ys)

reducer :: Int -> (Char, Char) -> Int
reducer acc (x,y) = acc + if x==y then 0 else 1
