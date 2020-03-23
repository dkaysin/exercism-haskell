module Minesweeper (annotate) where

import           Data.Vector (Vector)
import qualified Data.Vector as V


annotate :: [String] -> [String]
annotate [""] = [""]
annotate board = chunk stride $ map (processCell vec stride height) [0..len-1]
  where
    vec = vectorize board
    height = length board
    len = V.length vec
    stride = len `div` height
    chunk _ [] = []
    chunk n xs = take n xs : chunk n (drop n xs)

vectorize :: [[Char]] -> Vector Char
vectorize xs = V.concat $ map V.fromList xs

processCell :: Vector Char -> Int -> Int -> Int -> Char
processCell vec stride height n
  | vec V.! n == '*' = '*'
  | res == 0 = ' '
  | otherwise = head $ show res
  where
    (x0, y0) = linToMat stride n
    neighbPoints = map (matToLin stride)
      [(x, y) | x <- (+x0)<$>[-1..1], y <- (+y0)<$>[-1..1], x>=0, y>=0, x<stride, y<height]
    checkCell k
      | vec V.! k == '*' = 1 :: Int
      | otherwise = 0
    res = sum $ map checkCell neighbPoints

linToMat :: Int -> Int -> (Int, Int)
linToMat stride n = (n `mod` stride, n `div` stride)

matToLin :: Int -> (Int, Int) -> Int
matToLin stride (x,y) = y*stride + x
