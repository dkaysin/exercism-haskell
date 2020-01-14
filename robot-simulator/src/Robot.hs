module Robot
    ( Bearing(East,North,South,West)
    , bearing
    , coordinates
    , mkRobot
    , move
    ) where

data Bearing = North
             | East
             | South
             | West
             deriving (Eq, Enum, Show)

data Robot = Robot Bearing Point
             deriving (Show)

data Point = Point Integer Integer
             deriving (Eq, Show)

data Turn = L | R
            deriving (Eq, Show)

(+++) :: Point -> Point -> Point
(Point x1 y1) +++ (Point x2 y2) = Point (x1+x2) (y1+y2)

bearing :: Robot -> Bearing
bearing (Robot b _) = b

coordinates :: Robot -> (Integer, Integer)
coordinates (Robot _ (Point x y)) = (x, y)

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot direction (x, y) = Robot direction $ Point x y

move :: Robot -> String -> Robot
move =  foldl moveOne

moveOne :: Robot -> Char -> Robot
moveOne robot 'A' = advance robot
moveOne robot 'L' = turn L robot
moveOne robot 'R' = turn R robot
moveOne robot _   = robot

advance :: Robot -> Robot
advance (Robot b p) = Robot b $ p +++ delta
  where
    delta = case b of
      North -> Point    0    1
      East  -> Point    1    0
      South -> Point    0  (-1)
      West  -> Point  (-1)   0

turn :: Turn -> Robot -> Robot
turn t (Robot b p) = Robot (cycleTurn t b) p

cycleTurn :: Turn -> Bearing -> Bearing
cycleTurn L North = West
cycleTurn L b     = pred b
cycleTurn R West  = North
cycleTurn R b     = succ b
