module Change (findFewestCoins) where

import           Data.List

findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins  = flip minCoins'

type FuncSign = Integer -> Maybe [Integer]

minCoins :: [Integer] -> FuncSign -> FuncSign
minCoins coins mf v
  | v == 0 = Just []
  | v < 0 = Nothing
  | otherwise = minimumBy compareLength [(c:) <$> mf (v-c) | c <- coins]

compareLength :: Ord a => Maybe [a] -> Maybe [a] -> Ordering
compareLength (Just a) (Just b) = length a `compare` length b
compareLength a b               = b `compare` a -- order flipped to have Nothing > (Just _)

-- Structure with (Log n)-time reads can be used to further improve performance
memoList :: (FuncSign -> FuncSign) -> FuncSign
memoList f = get
  where get n
          | n < 0 = f get n
          | otherwise = (memo!!) $ fromInteger n
        memo = map (f get) [0..]

minCoins' :: [Integer] -> FuncSign
minCoins' coins = memoList (minCoins coins)
