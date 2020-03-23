module Roman (numerals) where

data RomanDigit =  M | D | C | L | X | V | I
                  deriving (Eq, Ord, Enum, Bounded, Show)

decToRomanDigit :: Integer -> RomanDigit
decToRomanDigit 1000 = M
decToRomanDigit 500  = D
decToRomanDigit 100  = C
decToRomanDigit 50   = L
decToRomanDigit 10   = X
decToRomanDigit 5    = V
decToRomanDigit 1    = I

numerals :: Integer -> Maybe String
numerals n
  | n <= 3*largest = Just $ concatMap (show.decToRomanDigit) $ concat $ digits largest n
  | otherwise = Nothing
  where largest = 1000

digits :: Integer -> Integer -> [[Integer]]
digits 1 n = [expand 1 n]
digits dv n = expand dv n : digits (dv `div` 10) (n-digit*dv)
  where
    digit = n `div` dv

expand :: Integer -> Integer -> [Integer]
expand dv n
  | d == 4 = (*dv) <$> [1, 5]
  | d == 9 = (*dv) <$> [1, 10]
  | d <= 3 = replicate (fromIntegral d) dv
  | d >= 5 = 5*dv : replicate (fromIntegral (d-5)) dv
  | otherwise = [d*dv]
  where
      d = n `div` dv
