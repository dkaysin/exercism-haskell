module Grains (square, total) where

import           Data.Maybe

square :: Integer -> Maybe Integer
square n
  | n >= 1 && n <= 64 = Just $ 2^(n-1)
  | otherwise         = Nothing

total :: Integer
total = sum $ map (fromMaybe 0 . square) [1..64]
