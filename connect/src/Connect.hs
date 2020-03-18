module Connect (Mark(..), winner) where

import           Control.Monad.State
import           Data.Map            (Map)
import qualified Data.Map            as M

data Mark = Cross | Nought | Null | CrossBoundary | NoughtBoundary deriving (Eq, Show)
type Index = (Int, Int)
type Grid = Map Index Mark

winner :: [String] -> Maybe Mark
winner board
  | winX = Just Cross
  | winO = Just Nought
  | otherwise = Nothing
  where
    winX = checkMarkWin (Cross, CrossBoundary) (-1,0)
    winO = checkMarkWin (Nought, NoughtBoundary) (0,-1)
    checkMarkWin (from, to) ix = evalState (connects (from, to) ix (return False)) (parseBlock board)


-- Checks neighbourhood of point for target marks,
-- then proceeds recursively until target mark is found
-- Visited indices are removed from the Grid
-- global Grid is tracked in the State monad

connects :: (Mark, Mark) -> Index -> State Grid Bool -> State Grid Bool
connects (from, to) ix checkLastS = do
  grid <- get
  let (neighbs, trimmed) = splitGrid grid ix
  put trimmed
  checkNext <- M.foldWithKey folder (return False) neighbs
  checkLast <- checkLastS
  return $ checkLast || checkNext
  where
    folder k m acc
      | m == to = return True
      | m == from = connects (from, to) k acc
      | otherwise = acc


-- Splits available grid into neighbourhood of given index and everything else
-- Returns both

splitGrid :: Grid -> Index -> (Grid, Grid)
splitGrid grid (x, y) = (neighbs, trimmed)
  where
    neighbIxs = [(i+x,y-1) | i<-[0,1]] ++ [(i+x,y) | i<-[-1,1]] ++ [(i+x,y+1) | i<-[-1,0]]
    neighbs = M.filterWithKey (\k _ -> k `elem` neighbIxs) grid
    trimmed = grid `M.difference` neighbs


-- We add padding on the sides to accomodate finding path:
-- for Crosses: from (-1,0) to CrossBoundary; and
-- for Noughts: from (0,-1) to NoughtBoundary

parseBlock :: [String] -> Grid
parseBlock xss
  = M.fromList $ concat [boundaryXleft, boundaryXright, boundaryYtop, boundaryYbottom
    , map (\(i,m) -> ((i `mod` stride, i `div` stride), m)) $ zip [0::Int ..] $ concat marks]
  where
    marks = map parseMarks xss
    stride = length $ head marks
    height = length marks
    boundaryXleft   = [( (-1,y),      Cross  ) | y <- [0..height-1]]
    boundaryXright  = [( (stride,y),  CrossBoundary  ) | y <- [0..height-1]]
    boundaryYtop    = [( (x,-1),      Nought ) | x <- [0..stride-1]]
    boundaryYbottom = [( (x,height),  NoughtBoundary ) | x <- [0..stride-1]]

parseMarks :: String -> [Mark]
parseMarks = map parseMark . words

parseMark :: String -> Mark
parseMark "X" = Cross
parseMark "O" = Nought
parseMark _   = Null
