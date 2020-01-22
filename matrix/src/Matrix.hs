module Matrix
    ( Matrix
    , cols
    , column
    , flatten
    , fromList
    , fromString
    , reshape
    , row
    , rows
    , shape
    , transpose
    ) where

import           Data.Maybe  (fromMaybe)
import           Data.Vector (Vector)
import qualified Data.Vector as V

data Matrix a = Matrix (Vector (Vector a))
  deriving (Eq, Show)

cols :: Matrix a -> Int
cols (Matrix vs) = fromMaybe 0 $ V.length <$> vs V.!? 0

column :: Int -> Matrix a -> Vector a
column n (Matrix vs) = V.map (V.! (n-1)) vs

flatten :: Matrix a -> Vector a
flatten (Matrix vs) = V.foldl (V.++) V.empty vs

fromList :: [[a]] -> Matrix a
fromList = Matrix . V.fromList . map V.fromList

fromString :: Read a => String -> Matrix a
fromString = Matrix . V.fromList . map (V.fromList . map read . words) . lines

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (r, c) (Matrix vs) = Matrix $ V.take r $ chunks c (V.head vs)

chunks :: Int -> Vector a -> Vector (Vector a)
chunks n v
  | V.length v < n = V.empty
  | otherwise = V.take n v `V.cons` chunks n (V.drop n v )

row :: Int -> Matrix a -> Vector a
row n (Matrix vs) = vs V.! (n-1)

rows :: Matrix a -> Int
rows (Matrix vs) = V.length vs

shape :: Matrix a -> (Int, Int)
shape mx = (rows mx, cols mx)

transpose :: Matrix a -> Matrix a
transpose mx = Matrix $ V.fromList $ map (`column` mx) [1..len]
  where len = cols mx
