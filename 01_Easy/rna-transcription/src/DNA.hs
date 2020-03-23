module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA (s:xs) = case encode s of
  Just e  -> case toRNA xs of
    Left pass -> Left pass
    Right xe  -> Right (e:xe)
  Nothing -> Left s
toRNA _ = Right ""

encode :: Char -> Maybe Char
encode 'G' = Just 'C'
encode 'C' = Just 'G'
encode 'T' = Just 'A'
encode 'A' = Just 'U'
encode _   = Nothing
