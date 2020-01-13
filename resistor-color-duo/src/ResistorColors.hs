module ResistorColors (Color(..), value) where

import           Data.Map (Map)
import qualified Data.Map as M

data Color =
    Black
  | Brown
  | Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Violet
  | Grey
  | White
  deriving (Eq, Show, Ord)

value :: (Color, Color) -> Int
value (a, b) = numA * 10 + numB
  where
    numA = colorCoding M.! a
    numB = colorCoding M.! b

colorCoding :: Map Color Int
colorCoding = M.fromList [
    (Black,   0)
  , (Brown,   1)
  , (Red,     2)
  , (Orange,  3)
  , (Yellow,  4)
  , (Green,   5)
  , (Blue,    6)
  , (Violet,  7)
  , (Grey,    8)
  , (White,   9)]
