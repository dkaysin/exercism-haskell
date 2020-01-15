module ProteinTranslation(proteins) where

import           Control.Monad (foldM)
import           Data.List     (find)

proteins :: String -> Maybe [String]
proteins xs = sequence $ either id id $ foldl reducer (Right []) $ map match $ chunks3 xs

match :: String -> Maybe (Maybe String)
match xs = snd <$> find (\ (patterns, _) -> xs `elem` patterns) dict

chunks3 :: String -> [String]
chunks3 = filter ((==n).length) . chunks n
  where n = 3

chunks :: Int -> String -> [String]
chunks _ [] = []
chunks n xs = take n xs : chunks n (drop n xs)

reducer :: Either [Maybe String] [Maybe String] -> Maybe (Maybe String) -> Either [Maybe String] [Maybe String]
-- fmap does not affect instances of Left, only instances of Right
reducer acc (Just (Just x)) = (++[Just x]) <$> acc
reducer acc Nothing         = (++[Nothing]) <$> acc
reducer acc (Just Nothing)  = Left $ either id id acc

dict :: [([String], Maybe String)]
dict = [
      (["AUG"], Just "Methionine")
    , (["UUU", "UUC"], Just "Phenylalanine")
    , (["UUA", "UUG"], Just "Leucine")
    , (["UCU", "UCC", "UCA", "UCG"], Just "Serine")
    , (["UAU", "UAC"], Just "Tyrosine")
    , (["UGU", "UGC"], Just "Cysteine")
    , (["UGG"], Just "Tryptophan")
    , (["UAA", "UAG", "UGA"], Nothing)
  ]
