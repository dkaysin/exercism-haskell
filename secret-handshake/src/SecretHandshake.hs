module SecretHandshake (handshake) where

import           Data.Bits ((.&.))

handshake :: Int -> [String]
handshake n
  | (binToDec 10000 .&. n)  > 0 = reverse res
  | otherwise = res
  where
    res = [xs | (pat, xs) <- moves, (binToDec pat .&. n) > 0]

binToDec :: Int -> Int
binToDec = convBases 2 10

convBases :: Int -> Int -> Int -> Int
convBases _ _ 0 = 0
convBases fromBase toBase n = (n `mod` toBase) + fromBase * convBases fromBase toBase (n `div` toBase)

moves :: [(Int, String)]
moves = [
      (1, "wink")
    , (10, "double blink")
    , (100, "close your eyes")
    , (1000, "jump")
  ]
