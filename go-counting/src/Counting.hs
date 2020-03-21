module Counting (
    Color(..),
    territories,
    territoryFor
) where

import           Control.Monad.State (State, execState, get, mapM_, modify,
                                      unless, when)
import           Data.List           (nub)
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Maybe          (mapMaybe)
import           Data.Set            (Set)
import qualified Data.Set            as S

data Color = Black | White | None deriving (Eq, Ord, Show)
type Coord = (Int, Int)
type Cell = (Coord, Color)
type Grid = Map Coord Color

territories :: [String] -> [(Set Coord, Maybe Color)]
territories board
  = nub $ mapMaybe (territoryFor board . fst) $ parseCells board

territoryFor :: [String] -> Coord -> Maybe (Set Coord, Maybe Color)
territoryFor board coord
  | null areaN = Nothing
  | null areaW && not (null areaB) = Just (areaN, Just Black)
  | null areaB && not (null areaW) = Just (areaN, Just White)
  | otherwise = Just (areaN, Nothing)
  where
    state = collectIntersection (parseGrid board) coord
    cellMap = groupSetPerColor $ execState state S.empty
    -- Count how many cells of each type are within intersection
    [areaB, areaW, areaN] = map (cellMap M.!) [Black, White, None]

-- Recursively goes through neighbours of the given point
-- Search branch is cut once reached "Black" or "White" cell
collectIntersection :: Grid -> Coord -> State (Set Cell) ()
collectIntersection grid curC = case M.lookup curC grid of
  Just color  -> do
    -- We keep track of visited cells inside the State monad
    -- Cells of any color can be added
    -- "None" cells will determine actual set of cells within the intersection
    --  "Black"/"White" cells will decide which player controls the intersection
    modify $ S.insert (curC, color)
    when (color == None) $ do
      excluded <- get
      -- We proceed only for cells that has not been visited previously
      let neighb = neighbSet curC `S.difference` S.map fst excluded
      -- If neighbourhood is not empty, evalate each neighbour that
      -- has not been evaluated before; otherwise - abort branch
      unless (null neighb) $ mapM_ (collectIntersection grid) neighb
  Nothing -> return ()

-- Aggregate cells within the given intersection per color
-- It is used in making a decision on the color of the intersection
groupSetPerColor :: Set Cell -> Map Color (Set Coord)
groupSetPerColor set = M.fromList $ map (\col ->
  (col, S.map fst (S.filter (\(_, c) -> c == col) set)))
    [Black, White, None]

-- Checks for invalid values of Coord are implemented in the
-- "collectIntersection" function
neighbSet :: Coord -> Set Coord
neighbSet (x,y) = S.fromList [ (x,y+1), (x,y-1), (x+1,y), (x-1,y) ]

-- Parsers

parseGrid :: [String] -> Grid
parseGrid = M.fromList . parseCells

parseCells :: [String] -> [Cell]
parseCells xxs = [
    ((i `mod` stride +1, i `div` stride +1), parseLetter s)
    | (i, s) <- zip [0..] xs
  ]
  where
    xs = concat xxs
    height = length xxs
    stride = length xs `div` height

parseLetter :: Char -> Color
parseLetter 'B' = Black
parseLetter 'W' = White
parseLetter _   = None
