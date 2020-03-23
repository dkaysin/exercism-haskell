module Phone (number) where

import           Data.Char (isDigit)

number :: String -> Maybe String
number xs
  | length res /= length targetPattern = Nothing
  | all (\(b, s) -> b s) (zip targetPattern res) = Just res
  | otherwise = Nothing
  where
    withCode = filter isDigit xs
    code = head withCode
    res = if cF code then tail withCode else withCode

targetPattern :: [Char -> Bool]
targetPattern = [cN, cX, cX,
                 cN, cX, cX,
                 cN, cX, cX, cX]

cF :: Char -> Bool
cF s = s `elem` ['1']

cN :: Char -> Bool
cN s = s `elem` ['2'..'9']

cX :: Char -> Bool
cX s = s `elem` ['0'..'9']
