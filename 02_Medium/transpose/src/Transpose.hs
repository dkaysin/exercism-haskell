module Transpose (transpose) where

import           Data.Maybe

transpose :: [String] -> [String]
transpose xxs
  | null (concat xxs) = []
  | otherwise = h : transpose t
  where
    h = strip ' ' $ map head' xxs
    t = extend [] $ map tail' xxs

strip :: Eq a => a -> [Maybe a] -> [a]
strip def xs = reverse $ extend def h
  where (_, h) = span (==Nothing) $ reverse xs

extend :: a -> [Maybe a] -> [a]
extend def =  map (fromMaybe def)

head' :: [a] -> Maybe a
head' []    = Nothing
head' (x:_) = Just x

tail' :: [a] -> Maybe [a]
tail' []     = Nothing
tail' (_:xs) = Just xs
