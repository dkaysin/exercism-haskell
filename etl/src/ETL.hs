module ETL (transform) where

import           Data.Char  (toLower)
import           Data.Map   (Map, fromList, toList)
import           Data.Tuple (swap)

transform :: Map a String -> Map Char a
transform d = fromList $ concatMap (unpack . swap) $ toList d

unpack :: Functor f => (f Char, a) -> f (Char, a)
unpack (fch, k) = (\ch -> (toLower ch, k)) <$> fch
