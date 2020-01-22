module House (rhyme) where

import           Data.List

rhyme :: String
rhyme = intercalate "\n" $ map part [1..12]

part :: Int -> String
part n = unlines $ take n [unwords (filter (/="") [a,b,c,d]) | (a,b,c,d) <- zip4 pron cactions the citems]
  where
    citems = reverse (take n items)
    cactions = "" : reverse (take (n-1) actions)

pron :: [String]
pron = "This is" : repeat "that"

actions :: [String]
actions = ["lay in","ate","killed","worried","tossed","milked","kissed","married","woke","kept","belonged to"]

the :: [String]
the = repeat "the"

items :: [String]
items = ["house that Jack built.","malt","rat","cat","dog","cow with the crumpled horn","maiden all forlorn","man all tattered and torn","priest all shaven and shorn","rooster that crowed in the morn","farmer sowing his corn","horse and the hound and the horn"]
