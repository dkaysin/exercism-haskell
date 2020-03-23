module Base (Error(..), rebase) where

data Error a = InvalidInputBase | InvalidOutputBase | InvalidDigit a
    deriving (Show, Eq)

rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inB outB digits
  | inB <= 1 = Left InvalidInputBase
  | outB <= 1 = Left InvalidOutputBase
  | otherwise = fromDec outB $ toDec inB digits

toDec :: Integral a => a -> [a] -> Either (Error a) a
toDec inB digits = sum <$> mapM processDigit ts
  where
    ts = zip (reverse digits) (iterate (*inB) 1)
    processDigit (d, m)
      | d >= inB = Left (InvalidDigit d)
      | d < 0 = Left (InvalidDigit d)
      | otherwise = Right (d*m)

fromDec :: Integral a => a -> Either (Error a) a -> Either (Error a) [a]
fromDec _ (Left e) = Left e
fromDec _ (Right 0) = Right []
fromDec outB n      = (++) <$> fromDec outB dv <*> ((:[]) <$> md)
  where
    dv = (`div` outB) <$> n
    md = (`mod` outB) <$> n
