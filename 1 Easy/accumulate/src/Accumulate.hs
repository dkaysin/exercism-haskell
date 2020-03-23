module Accumulate (accumulate) where

accumulate :: (a -> b) -> [a] -> [b]
accumulate f lst = case lst of
  x:xs -> f x : accumulate f xs
  []   -> []
