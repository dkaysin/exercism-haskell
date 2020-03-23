module Scrabble (scoreLetter, scoreWord) where

import           Data.Char (toUpper)
import           Data.List (find)

scoreLetter :: Char -> Integer
scoreLetter s = case scoreMaybe of
  Just score -> snd score
  Nothing    -> 0
  where scoreMaybe = find (\ (a, _) -> toUpper s `elem` a) scoring

scoreWord :: String -> Integer
scoreWord = sum . map scoreLetter

scoring :: [(String, Integer)]
scoring = [
      ("AEIOULNRST", 1)
    , ("DG", 2)
    , ("BCMP", 3)
    , ("FHVWY", 4)
    , ("K", 5)
    , ("JX", 8)
    , ("QZ", 10)
  ]
