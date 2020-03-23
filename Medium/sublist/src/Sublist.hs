module Sublist (sublist) where

import           Data.List

sublist :: Eq a => [a] -> [a] -> Maybe Ordering
sublist xs ys
  | xINy && yINx = Just EQ
  | xINy = Just LT
  | yINx = Just GT
  | otherwise = Nothing
  where
    xINy = xs `isContained` ys
    yINx = ys `isContained` xs

isContained :: Eq a => [a] -> [a] -> Bool
isContained [] _  = True
isContained _ []  = False
isContained xa xb = xa `isPrefixOf` xb || isContained xa (tail xb)
