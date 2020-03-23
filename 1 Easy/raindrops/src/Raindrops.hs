module Raindrops (convert) where

convert :: Int -> String
convert n
  | res == "" = show n
  | otherwise = res
  where
    a `divides` b = b `mod` a == 0
    res = concatMap (\(f, str) -> if f `divides` n then str else "") factors

factors :: [(Int, String)]
factors = [
      (3, "Pling")
    , (5, "Plang")
    , (7, "Plong")
  ]
