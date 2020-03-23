module Brackets (arePaired) where

import           Data.List

arePaired :: String -> Bool
arePaired xs = case res of
  Just "" -> True
  _       -> False
  where
    res = foldl' folder (Just "") $ filter (`elem` dict) xs
    dict = opens ++ map pair opens

folder :: Maybe String -> Char -> Maybe String
folder acc next
  | next `elem` opens = (next:) <$> acc
  | otherwise = acc >>= go
  where
    go []     = Nothing
    go (s:xs)
      | pair s == next = Just xs
      | otherwise = Nothing

opens :: String
opens = "[({"

pair :: Char -> Char
pair '[' = ']'
pair '(' = ')'
pair '{' = '}'
pair _   = '_'
