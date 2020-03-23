module Atbash (decode, encode) where

import           Data.Char (toLower)
import           Data.Map  (Map)
import qualified Data.Map  as M

decode :: String -> String
decode = genericCoder

encode :: String -> String
encode = unwords . chunks 5 . genericCoder

genericCoder :: String -> String
genericCoder = concatMap (\s -> M.findWithDefault [s] s cipher)

cipher :: Map Char String
cipher = M.fromList $ lowerabc ++ higherabc ++ discard
  where
    lowerabc = zip ['a'..'z'] $ reverse $ map (:[]) ['a'..'z']
    higherabc = zip ['A'..'Z'] $ reverse $ map (:[]) ['a'..'z']
    discard = zip [' ', ',', '.'] $ repeat ""

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = take n xs : chunks n (drop n xs)
