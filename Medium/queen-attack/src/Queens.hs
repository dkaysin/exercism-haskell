module Queens (boardString, canAttack) where

import           Data.Array

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString white black = unlines [unwords [plot x y | y <- board] | x <- board]
  where
    plot x y
      | parse black == (x,y) = "B"
      | parse white == (x,y) = "W"
      | otherwise = "_"

parse :: Maybe (Int, Int) -> (Int, Int)
parse (Just (x, y)) = (x, y)
parse Nothing       = (-1, -1)

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack queenA queenB = queenA `hits` queenB
  where
    hits (px, py) (x, y) = abs(x-px) == abs(y-py)
                        || y == py
                        || x == px

board :: [Int]
board = [0..7]
