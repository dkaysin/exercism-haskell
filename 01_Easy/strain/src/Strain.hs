module Strain (keep, discard) where

discard :: (a -> Bool) -> [a] -> [a]
discard p = keep (not.p)

keep :: (a -> Bool) -> [a] -> [a]
keep p (x:xs) = [x | p x] ++ keep p xs
keep _ []     = []
