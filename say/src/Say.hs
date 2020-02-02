module Say (inEnglish) where

import           Data.List

inEnglish :: Integer -> Maybe String
inEnglish 0 = Just "zero"
inEnglish n
  | n < 0 = Nothing
  | n < 100 && n > 0 = Just $ zeroTo99 n
  | otherwise = unwords <$> (filter (/="") <$> sequence res)
  where
    res = concatMap (\(i, pre, post) -> [pre, inEnglish i, post])
      $ filter (\(i,_,_) -> i/=0) $ split multiples n

split :: [(Integer, Maybe String, Maybe String)] -> Integer -> [(Integer, Maybe String, Maybe String)]
split [] _ = []
split mults n = (cur, mPre, mPost) : split (tail mults) (n - cur * mInt)
  where
    (mInt, mPre, mPost) = head mults
    cur = n `div` mInt

multiples :: [(Integer, Maybe String, Maybe String)]
multiples = [
      (10^9,  Just "",      Just "billion")
    , (10^6,  Just "",      Just "million")
    , (1000,  Just "",      Just "thousand")
    , (100,   Just "",      Just "hundred")
    , (1,     Just "and",   Just "")
  ]

zeroTo99 :: Integer -> String
zeroTo99 1  = "one"
zeroTo99 2  = "two"
zeroTo99 3  = "three"
zeroTo99 4  = "four"
zeroTo99 5  = "five"
zeroTo99 6  = "six"
zeroTo99 7  = "seven"
zeroTo99 8  = "eight"
zeroTo99 9  = "nine"
zeroTo99 10 = "ten"
zeroTo99 11 = "eleven"
zeroTo99 12 = "twelve"
zeroTo99 13 = "thirteen"
zeroTo99 14 = "fourteen"
zeroTo99 15 = "fifteen"
zeroTo99 16 = "sixteen"
zeroTo99 17 = "seventeen"
zeroTo99 18 = "eighteen"
zeroTo99 19 = "nineteen"
zeroTo99 20 = "twenty"
zeroTo99 30 = "thirty"
zeroTo99 40 = "forty"
zeroTo99 50 = "fifty"
zeroTo99 60 = "sixty"
zeroTo99 70 = "seventy"
zeroTo99 80 = "eighty"
zeroTo99 90 = "ninety"
zeroTo99 n
  | n <= 99 && n > 0 = intercalate "-" [
        zeroTo99 (n `div` 10 * 10)
      , zeroTo99 (n `mod` 10)
    ]
  | otherwise = error "Unexpected integer"
