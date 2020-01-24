module Series (slices) where

import           Data.Char (digitToInt)
import           Data.List (tails)

slices :: Int -> String -> [[Int]]
slices n = filter ((==n).length) . map (take n) . tails . map digitToInt
