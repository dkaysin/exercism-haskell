module POV (fromPOV, tracePathBetween) where

import           Data.List
import           Data.Tree

fromPOV :: Eq a => a -> Tree a -> Maybe (Tree a)
fromPOV targetVal oldRoot
  = foldl flipRoot oldRoot . tail <$> findPathFromRoot targetVal oldRoot

tracePathBetween :: Eq a => a -> a -> Tree a -> Maybe [a]
tracePathBetween fromNode toNode tree
  = fmap getVal <$> (findPathFromRoot toNode =<< fromPOV fromNode tree)

-- Functions find path(s) from the root to the node with a given value

findPathFromRoot :: Eq a => a -> Tree a -> Maybe [Tree a]
findPathFromRoot val node = safeHead $ findPathsFromRoot val node

findPathsFromRoot :: Eq a => a -> Tree a -> [[Tree a]]
findPathsFromRoot val node@(Node a branches)
  | val == a = [[node]]
  | otherwise = map (node:) (findPathsFromRoot val =<< branches)

-- Operations on nodes

flipRoot :: Eq a => Tree a -> Tree a -> Tree a
flipRoot oldRoot newRoot = addToBranches (removeFromBranches newRoot oldRoot) newRoot

addToBranches :: Eq a => Tree a -> Tree a -> Tree a
addToBranches newBranch (Node a xa) = Node a (newBranch:xa)

removeFromBranches :: Eq a => Tree a -> Tree a -> Tree a
removeFromBranches branch (Node a xa) = Node a (delete branch xa)

-- Misc Functions

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (a:_) = Just a

getVal :: Tree a -> a
getVal (Node a _) = a
