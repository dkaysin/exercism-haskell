module Spiral (spiral) where

import           Data.Array

data Turtle = Turtle Point Orientation (Array Point Int) deriving (Show)
empty :: Int -> Turtle
empty size = Turtle (Point 1 1) Rght (array (Point 1 1, Point size size) [ (Point x y, 0) | y <- [1..size],  x <- [1..size]])

data Point = Point Int Int deriving (Eq, Ord, Ix, Show)
(+++) :: Point -> Point -> Point
(Point x1 y1) +++ (Point x2 y2) = Point (x1 + x2) (y1 + y2)

data Orientation = Rght | Dwn | Lft | Up deriving (Enum, Show)
succ' :: Orientation -> Orientation
succ' Up = Rght
succ' x  = succ x

orientToPoint :: Orientation -> Point
orientToPoint Rght = Point 1 0
orientToPoint Dwn  = Point 0 1
orientToPoint Lft  = Point (-1) 0
orientToPoint Up   = Point 0 (-1)

spiral :: Int -> [[Int]]
spiral size = pprint $ foldr next (empty size) $ reverse [1..size*size]

pprint :: Turtle -> [[Int]]
pprint (Turtle _ _ arr) = [[ arr ! Point x y | x <- [1..size]] | y <- [1..size]]
  where
    (_, Point size _) = bounds arr

next :: Int -> Turtle -> Turtle
next n (Turtle p orient arr) = Turtle (p +++ dp) newOrient (arr//[(p, n)])
  where
    testPoint = p +++ orientToPoint orient
    newOrient
      | testPoint `notElem` indices arr || arr ! testPoint /= 0 = succ' orient
      | otherwise = orient
    dp = orientToPoint newOrient
