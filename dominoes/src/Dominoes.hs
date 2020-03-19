module Dominoes (chain) where

import           Data.List

-- We create a custom data type "Tile" that behaves like a normal Dominoes tile:
-- Additional Bool parameter holds the latest orientation of the tile
data Tile = Null | Valid (Int,Int) Bool
-- Tiles are equal regardless of their orientation
instance Eq Tile where
  (Valid d1 _) == (Valid d2 _) = d1 == d2
  _ == _ = True

makeTile :: (Int,Int) -> Tile
makeTile (a,b) = Valid (a,b) False

-- We present tile oriented according to the Bool value
presentTile :: Tile -> (Int,Int)
presentTile (Valid d s) = if s then swap d else d
presentTile _           = (-1,-1)

-- Swaps the tile, which helps when matching to another given tile
swapTile :: Tile -> Tile
swapTile (Valid d s) = Valid d (not s)
swapTile t           = t

-- List here is used as a Maybe-like structure
matchesNextTile :: Tile -> Tile -> [Tile]
matchesNextTile t0 t1
  | s == a = [t1]
  | s == b = [swapTile t1]
  | otherwise = []
  where
    (_,s) = presentTile t0
    (a,b) = presentTile t1

outsideMatches :: Tile -> Tile -> Bool
outsideMatches t1 t2 = fst (presentTile t1) == snd (presentTile t2)

chain :: [(Int, Int)] -> Maybe [(Int, Int)]
chain [] = Just []
chain dominoes = map presentTile <$> safeHead fltrd
  where
    chains = filter (\xd -> length xd == length dominoes) $ getChains Null $ map makeTile dominoes
    fltrd = filter (\xt -> outsideMatches (head xt) (last xt)) chains

getChains :: Tile -> [Tile] -> [[Tile]]
getChains currT xt
  | null matchingTs = [[]]
  | otherwise = do
      nextT <- matchingTs
      -- Difference ('\\') on lists removes only first occurences
      -- and is based on the instance of '=='
      path <- getChains nextT (xt \\ [nextT])
      return $ nextT : path
  where matchingTs = filterMatching currT xt

filterMatching :: Tile -> [Tile] -> [Tile]
filterMatching Null xt = xt
filterMatching t xd    = concatMap (matchesNextTile t) xd

-- Misc functions
-- (they must be in one of the libraries, but I am too lazy to seqrch for them)

safeHead :: [a] -> Maybe a
safeHead xa
  | null xa = Nothing
  | otherwise = Just (head xa)

swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)
