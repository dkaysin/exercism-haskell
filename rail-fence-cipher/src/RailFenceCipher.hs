module RailFenceCipher (encode, decode) where

import           Data.Char
import           Data.List
import           Data.Maybe

encode :: Int -> String -> String
encode n xs = genericEncode n $ filter isAlpha xs

genericEncode :: Int -> [a] -> [a]
genericEncode n xs = concat [oneLine k | k <- [0..maxk]]
  where
    maxk = n - 1
    oneLine k = [s | (s, m) <- zip xs [0..]
      , lineNumber maxk m == k]

lineNumber :: Int -> Int -> Int
lineNumber maxk k
  | p > maxk = 2 * maxk - p
  | otherwise = p
  where
    dv = 2 * maxk
    p = k `mod` dv

decode :: Int -> String -> String
decode n xs = map (xs!!) index
  where
    len = length xs
    template = genericEncode n [1..len]
    index = fromMaybe (error "Error while building index in Decode function")
      $ mapM (`elemIndex` template) [1..len]
