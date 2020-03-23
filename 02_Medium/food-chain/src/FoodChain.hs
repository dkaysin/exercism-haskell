module FoodChain (song) where

import           Data.List

song :: String
song =  intercalate "\n" $ map block [0..7]

block :: Int -> String
block n = unlines $ filter (/="")
  $ concatMap ($n) [fstline, sndline, thrdline, lastline]

animals :: Int -> (String, String)
animals n =  zip (tail dict ++ [""]) dict !! n
  where
    dict = ["fly", "spider", "bird", "cat", "dog", "goat", "cow", "horse"]

fstline :: Int -> [String]
fstline n = ["I know an old lady who swallowed a " ++ snd (animals n) ++ "."]

sndline :: Int -> [String]
sndline n = [dict !! n]
  where
    dict = [
        ""
        , "It wriggled and jiggled and tickled inside her."
        , "How absurd to swallow a bird!"
        , "Imagine that, to swallow a cat!"
        , "What a hog, to swallow a dog!"
        , "Just opened her throat and swallowed a goat!"
        , "I don't know how she swallowed a cow!"
        , ""
      ]

thrdline :: Int -> [String]
thrdline 7 = [""]
thrdline n = concatMap (\k -> ["She swallowed the " ++ fst (animals k) ++ " to catch the " ++ snd (animals k) ++ suffix k ++ "."]) (reverse [0..n-1])
  where
    suffix 1 = " that wriggled and jiggled and tickled inside her"
    suffix _ = ""

lastline :: Int -> [String]
lastline 7 = ["She's dead, of course!"]
lastline _ = ["I don't know why she swallowed the fly. Perhaps she'll die."]
