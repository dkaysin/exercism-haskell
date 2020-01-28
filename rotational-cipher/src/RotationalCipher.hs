module RotationalCipher (rotate) where

import           Data.Maybe (fromMaybe)

rotate :: Int -> String -> String
rotate n = map look
  where
    shift k xn = take (length xn) $ drop k $ cycle xn
    cypher xs = zip xs $ shift n xs
    dict = cypher ['a'..'z'] ++ cypher ['A'..'Z']
    look s = fromMaybe s $ lookup s dict
