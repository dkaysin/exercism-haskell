module Palindromes (largestPalindrome, smallestPalindrome) where

import           Data.List

type Factors = (Integer, Integer)
type Entry = (Integer, [Factors])

largestPalindrome :: Integer -> Integer -> Maybe Entry
largestPalindrome = genericPalindrome maximum

smallestPalindrome :: Integer -> Integer -> Maybe Entry
smallestPalindrome = genericPalindrome minimum

genericPalindrome :: ([Integer] -> Integer)  -> Integer -> Integer -> Maybe (Integer, [Factors])
genericPalindrome f a b = do
  let bounds = (a, b)
  pal <- findPalindrome f bounds
  let factors = listFactors bounds pal
  return (pal, factors)

listFactors :: (Integer, Integer) -> Integer -> [Factors]
listFactors (mn, mx) n = filter (\(x, y) -> x*y == n) [(a,b) | a <- ms, b <- ms, a <= b]
  where ms = [mn..mx]

findPalindrome :: ([Integer] -> Integer) -> (Integer, Integer) -> Maybe Integer
findPalindrome f (a, b) = f <$> maybeList [x*y | x <- ms, y <- ms, isPalindrome (x*y)]
  where ms = [a..b]

isPalindrome :: Integer -> Bool
isPalindrome n = fwd == rev
  where
    fwd = show n
    rev = reverse fwd

maybeList :: [a] -> Maybe [a]
maybeList []  = Nothing
maybeList lst = Just lst
