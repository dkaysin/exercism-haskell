module Isogram (isIsogram) where

import           Data.Char (isAlpha)
import           Data.Map  (Map)
import qualified Data.Map  as M
import           Data.Text (Text)
import qualified Data.Text as T

isIsogram :: Text -> Bool
isIsogram xs
  | M.foldl (\acc x -> acc && (x<=1)) True res = True
  | otherwise = False
  where res = T.foldl reducer M.empty $ T.toLower xs

reducer :: Map Char Int -> Char -> Map Char Int
reducer acc x
  | M.notMember x acc = M.insert x 1 acc
  | isAlpha x= M.adjust (+1) x acc
  | otherwise           = acc
