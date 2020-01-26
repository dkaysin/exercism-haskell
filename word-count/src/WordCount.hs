module WordCount (wordCount) where

import           Data.Map   (Map)
import qualified Data.Map   as M
import           Data.Maybe
import           Data.Text  (Text, map)
import qualified Data.Text  as T

wordCount :: Text -> [(Text, Int)]
wordCount xs = M.toList
  $ foldr (M.alter count . clear) M.empty
  $ T.words $ T.map allowed $ T.toLower xs

count :: Maybe Int -> Maybe Int
count Nothing  = Just 1
count (Just x) = Just $ x + 1

clear :: Text -> Text
clear xs = fromMaybe xs $ T.stripPrefix (T.pack "'")
  $ fromMaybe xs $ T.stripSuffix (T.pack "'") xs

allowed :: Char -> Char
allowed s
  | s `elem` dict = s
  | otherwise = ' '
  where
    dict = ['a'..'z'] ++ ['0'..'9'] ++ "' "
