module Anagram (anagramsFor) where

import           Data.Char (toLower)
import           Data.List (sort)

anagramsFor :: String -> [String] -> [String]
anagramsFor xs = filter (\a -> (prepare a == prepare xs) && (lower a /= lower xs))
  where
    lower = map toLower
    prepare = sort.lower
