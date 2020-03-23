
module Pangram (isPangram) where

import qualified Data.Char as C

isPangram :: String -> Bool
isPangram text = all (`elem` map C.toLower text) "abcdefghijklmnopqrstuvwxyz"
