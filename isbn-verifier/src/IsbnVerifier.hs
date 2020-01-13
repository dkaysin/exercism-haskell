module IsbnVerifier (isbn) where

import           Data.Char (digitToInt, isDigit)
import           Data.List (nub)

isbn :: String -> Bool
isbn xs
  | correctLength && correctSymbols && correctCheckSum = True
  | otherwise = False
  where
    fld = filter (`elem` sym) xs
    correctLength = length fld == length pat
    correctSymbols = and [ s `elem` p | (p, s) <- zip pat fld]
    correctCheckSum = checkSum $ map conv fld

checkSum :: [Int] -> Bool
checkSum xn = p `mod` 11 == 0
  where
    p = sum [x*y | (x,y) <- zip xn [10, 9, 8, 7, 6, 5, 4, 3, 2, 1]]

sym :: [Char]
sym = nub $ concat pat

pat :: [[Char]]
pat = [fN, fN, fN, fN, fN, fN, fN, fN, fN, fX]

fN :: [Char]
fN = ['0'..'9']

fX :: [Char]
fX = ['0'..'9'] ++ ['X']

conv :: Char -> Int
conv s
  | isDigit s = digitToInt s
  | s == 'X' = 10
  | otherwise = 0
