module Diamond (diamond) where

diamond :: Char -> Maybe [String]
diamond s
    | s `elem` ['A'..'Z'] = Just $ formBlock3 s
    | otherwise = Nothing

formBlock3 ::  Char -> [String]
formBlock3 s = map (\xs -> reverse xs ++ tail xs) (formBlock2 s)

formBlock2 ::  Char -> [String]
formBlock2 s = formBlock1 s ++ (tail . reverse . formBlock1) s

formBlock1 :: Char -> [String]
formBlock1 s = map (formLine s len) [0..len-1]
  where
    len = length ['A'..s]

formLine :: Char -> Int -> Int -> String
formLine ch len n = (addPostfix (len-n-1) . addPrefix n) [arr!!n]
  where
    arr = ['A'..ch]

addPrefix :: Int -> String -> String
addPrefix k xs = replicate k ' ' ++ xs

addPostfix :: Int -> String -> String
addPostfix k xs = xs ++ replicate k ' '
