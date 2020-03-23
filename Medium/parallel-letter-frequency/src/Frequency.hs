module Frequency (frequency) where

import           Control.Parallel (par)
import           Data.Char        (isAlpha)
import           Data.Map         (Map, empty, insertWith, unionWith)
import           Data.Text        (Text, filter, foldl', toLower, unwords)
import           Prelude          hiding (filter, unwords)

frequency :: Int -> [Text] -> Map Char Int
frequency nWorkers texts = worker $ chunks nChunks texts
  where
    nPieces = length texts
    nChunks = nPieces `div` (min nPieces nWorkers)

worker :: [[Text]] -> Map Char Int
worker [] = empty
worker xxs = unionWith (+) (p `par` p) $ worker $ tail xxs
  where
    p = foldl' (\acc x -> insertWith (+) x (1::Int) acc) empty
      $ filter isAlpha $ toLower $ unwords $ head xxs

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = take n xs : chunks n (drop n xs)
