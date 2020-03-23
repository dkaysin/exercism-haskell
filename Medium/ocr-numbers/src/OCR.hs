module OCR (convert) where

import           Data.List
import           Data.Maybe

convert :: String -> String
convert xs = intercalate "," $ map concat res
  where
    res = map (map ocr) $ dDigits $ dSubLines xs

type Digit = [Points] -- Each Vertline represents 1 digit
type Points = [String] -- Each "point" is three chars long

dDigits :: [Points] -> [Digit]
dDigits = map transpose . chunks 4

dSubLines :: String -> [Points]
dSubLines = map (chunks 3) . lines

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = take n xs : chunks n (drop n xs)

ocr :: [String] -> String
ocr xs = fromMaybe "?" $ show . snd <$> find (\(k, _) -> k == xs) dict
  where
    dict = [
        (["   "
        , "  |"
        , "  |"
        , "   "], 1)
      , ([" _ "
        , " _|"
        , "|_ "
        , "   "], 2)
      , ([" _ "
        , " _|"
        , " _|"
        , "   "], 3)
      , (["   "
        , "|_|"
        , "  |"
        , "   "], 4)
      , ([" _ "
        , "|_ "
        , " _|"
        , "   "], 5)
      , ([" _ "
        , "|_ "
        , "|_|"
        , "   "], 6)
      , ([" _ "
        , "  |"
        , "  |"
        , "   "], 7)
      , ([" _ "
        , "|_|"
        , "|_|"
        , "   "], 8)
      , ([" _ "
        , "|_|"
        , " _|"
        , "   "], 9)
      , ([" _ "
        , "| |"
        , "|_|"
        , "   "], 0)
      ]
