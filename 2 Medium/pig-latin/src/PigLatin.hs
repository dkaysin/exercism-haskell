module PigLatin (translate) where

import           Data.List

nnul = not.null

translate :: String -> String
translate = unwords . map translateWord . words

translateWord :: String -> String
translateWord word@(s:xs)
  | nnul vowsPrefixMatched = word ++ "ay"
  | otherwise = b ++ a ++ "ay"
  where
    vowsPrefixMatched = filter (`isPrefixOf` word) vowelPrefixes
    (a, b) = splitAt (length consPrefix) word
    consPrefix
      | s `elem` consToVowel = s : go xs
      | otherwise = go word

go :: String -> String
go [] = []
go word@(s:xs)
  | nnul consPrefixMatched = head consPrefixMatched
  | isCons s = s : go xs
  | otherwise = []
  where
    consPrefixMatched = filter (`isPrefixOf` word) consPrefixes

isVowel :: Char -> Bool
isVowel s = s `elem` vowels

isCons :: Char -> Bool
isCons s = s `elem` consonants

vowels :: String
vowels = "euioay"

vowelPrefixes :: [String]
vowelPrefixes = ["xr", "yt"]

consonants :: String
consonants = "qwrtpsdfghjklzxcvbnm"

consPrefixes :: [String]
consPrefixes = ["qu"]

consToVowel :: String
consToVowel = "y"
