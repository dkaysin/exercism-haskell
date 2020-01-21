module Series (Error(..), largestProduct) where

import           Data.Char (digitToInt, isDigit)
import           Data.List (tails)

data Error = InvalidSpan | InvalidDigit Char deriving (Show, Eq)

largestProduct :: Int -> String -> Either Error Integer
largestProduct size digits
  | size > length digits = Left InvalidSpan
  | size < 0 = Left InvalidSpan
  | otherwise = maximum <$> mapM ((product<$>).sequence) subseqs
  where
    s = fromIntegral size
    subseqs =
      filter (\x -> length x == s)
      $ map (take s)
      $ tails
      $ map convDigit digits

convDigit :: Char -> Either Error Integer
convDigit s
  | (not.isDigit) s = Left (InvalidDigit s)
  | otherwise = Right $ fromIntegral $ digitToInt s
