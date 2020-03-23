module RunLength (decode, encode) where

import           Data.Char (isDigit)

-- ENCODE
encode :: String -> String
encode xs = split (breaksEncode xs) xs

split :: [Int] -> String -> String
split _ []      = ""
split (n:xn) xs = cur ++ split (subtract n <$> xn) (drop n xs)
  where
    cur
      | n == 1 = [head xs]
      | otherwise = show n ++ [head xs]
split _ _ = ""

breaksEncode :: String -> [Int]
breaksEncode xs = breaks xb
  where xb = zipWith (/=) xs (tail xs ++ "0")

breaks :: [Bool] -> [Int]
breaks xb = [n | (b, n) <- zip xb [1..], b]

-- DECODE
decode :: String -> String
decode (s:xs) = replicate n ch ++ decode rest
  where
    str = s:xs
    nString = getNumber str
    n = if nString == "" then 1 else read nString :: Int
    (ch:rest) = drop (length nString) str
decode _ = ""

getNumber :: String -> String
getNumber (s:xs)
  | isDigit s = s : getNumber xs
  | otherwise = ""
getNumber _ = ""
