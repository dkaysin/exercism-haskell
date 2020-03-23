module BinarySearch (find) where

import           Data.Array

find :: Ord a => Array Int a -> a -> Maybe Int
find arr x = go arr x mn mx
  where (mn, mx) = bounds arr

go :: Ord a => Array Int a -> a -> Int -> Int -> Maybe Int
go arr x mn mx
  | k < mn || k > mx = Nothing
  | otherwise = case compare x (arr ! k) of
    EQ -> Just k
    LT -> go arr x mn (k-1)
    GT -> go arr x (k+1) mx
  where
    k = ((mx - mn) `div` 2) + mn
