module Proverb(recite) where

import           Data.List (intercalate, tails)

recite :: [String] -> String
recite xss
  | null xss = ""
  | otherwise = intercalate "\n" $ firstPart ++ secondPart
  where
    firstPart = foldr (\ a acc -> line a ++ acc) []
      $ filter ((==2).length) $ map (take 2) $ tails xss
    secondPart = [unwords ["And all for the want of a", head xss] ++ "."]

line :: [String] -> [String]
line [first, second] = [unwords ["For want of a", first, "the", second, "was lost."]]
line _ = []
