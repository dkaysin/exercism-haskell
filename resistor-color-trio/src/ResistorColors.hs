module ResistorColors (Color(..), Resistor(..), label, ohms) where

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
  deriving (Eq, Ord, Show, Enum, Bounded)

newtype Resistor = Resistor { bands :: (Color, Color, Color) }
  deriving Show

label :: Resistor -> String
label res = show (if bDiv then nOhms `div` vMult else nOhms) ++ " " ++ suffMult
  where
    nOhms = ohms res
    t = head $ filter (\(v, _, _) -> nOhms >= v) ohmAbbrv
    (vMult, bDiv, suffMult) = t

ohms :: Resistor -> Int
ohms res = (numA*10 + numB) * 10^numC
  where
    (a, b, c) = bands res
    [numA, numB, numC] = (colorCoding M.!) <$> [a, b, c]

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
    , (White,   9)
  ]

ohmAbbrv :: [(Int, Bool, String)]
ohmAbbrv = [
      (10^9,  True,   "gigaohms")
    , (10^6,  True,   "megaohms")
    , (10^3,  True,   "kiloohms")
    , (0,     False,  "ohms")
  ]
