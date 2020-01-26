module CryptoSquare (encode) where

import           Data.Char
import           Data.List

encode :: String -> String
encode xs = unwords $ transpose $ chunks n str
  where
    n = ncols $ length str
    str = map toLower $ filter isAlphaNum xs

ncols :: Int -> Int
ncols 0 = 0
ncols m = head [c | c <- [1 .. m], r <- [c-1 .. c]
    , c * r >= m
    , c >= r
    , c - r <= 1
  ]

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = take n xs : chunks n (drop n xs)
