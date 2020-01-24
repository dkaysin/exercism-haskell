module Matrix (saddlePoints) where

import           Data.Array

saddlePoints :: (Ix i, Ord e, Eq e) => Array (i,i) e -> [(i,i)]
saddlePoints arr = map fst $ filter saddle $ assocs arr
  where
    saddle ((r, c), e) = (maxR r == e) && (minC c == e)
    maxR = maximum . getRow arr
    minC = minimum . getCol arr

getRow :: Ix i => Array (i,i) e -> i -> [e]
getRow arr x = [e | ((r, _), e) <- assocs arr, r == x]

getCol :: Ix i => Array (i,i) e -> i -> [e]
getCol arr x = [e | ((_, c), e) <- assocs arr, c == x]
