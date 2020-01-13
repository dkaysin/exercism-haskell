module Beer (song) where

import           Data.Char (toUpper)

song :: String
song = concatMap line $ reverse [0..99]

line :: Int -> String
line n = bU ++ otw ++ ", " ++ b ++ ".\n"
         ++ action n ++ ", " ++ bn ++ otw ++ ".\n"
         ++ divider n
  where
    b = bob n
    bU = (toUpper.head) b : tail b
    bn = bob (n-1)
    otw = " on the wall"

bob :: Int -> String
bob 1    = "1 bottle of beer"
bob 0    = "no more bottles of beer"
bob (-1) = "99 bottles of beer"
bob n    = show n ++ " bottles of beer"

action :: Int -> String
action 0 =  "Go to the store and buy some more"
action n =  "Take " ++ pron n ++ " down and pass it around"

pron :: Int -> String
pron 1 = "it"
pron _ = "one"

divider :: Int -> String
divider 0 = ""
divider _ = "\n"
