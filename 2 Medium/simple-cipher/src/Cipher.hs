module Cipher (caesarDecode, caesarEncode, caesarEncodeRandom) where

import           Data.Char
import           System.Random

(+++) :: Char -> Char -> Char
a +++ b
  | a `elem` ['a'..'z'] = numToChar $ (charToNum a + charToNum b) `mod` 26
  | otherwise = a

(!++) :: Char -> Char -> Char
a !++ b
  | a `elem` ['a'..'z'] = numToChar $ (charToNum a - charToNum b) `mod` 26
  | otherwise = a

charToNum :: Char -> Int
charToNum s = length ['a'..s] - 1 -- Map can be used to achieve O(log n)

numToChar :: Int -> Char
numToChar n = ['a'..'z'] !! n -- Map can be used to achieve O(log n)

caesarDecode :: String -> String -> String
caesarDecode = genericEncode (flip (!++))

caesarEncode :: String -> String -> String
caesarEncode = genericEncode (+++)

genericEncode :: (Char -> Char -> Char) -> String -> String -> String
genericEncode f key xs = zipWith f (cycle key) $ map toLower xs

caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom xs = do
  key <- replicate 100 <$> getStdRandom (randomR ('a', 'z'))
  return (key, caesarEncode key xs)
