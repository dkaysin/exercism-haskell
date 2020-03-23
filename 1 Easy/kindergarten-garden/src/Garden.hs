module Garden
    ( Plant (..)
    , garden
    , lookupPlants
    ) where

import           Data.List  (transpose)
import           Data.Map   (Map)
import qualified Data.Map   as M
import           Data.Maybe (fromMaybe)

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

data Garden = Garden (Map String [Char])
  deriving (Show)

garden :: [String] -> String -> Garden
garden students = Garden . M.fromList . zip students . map concat . transpose . map chunks . lines

chunks :: String -> [String]
chunks [] = []
chunks xs = take 2 xs : chunks (drop 2 xs)

lookupPlants :: String -> Garden -> [Plant]
lookupPlants student (Garden gdn) = fromMaybe [] plantsMaybe
  where
    plantsMaybe = M.lookup student gdn >>= mapM charToPlant

charToPlant :: Char -> Maybe Plant
charToPlant s = M.lookup s dict
  where
    dict = M.fromList [
          ('C', Clover)
        , ('G', Grass)
        , ('R', Radishes)
        , ('V', Violets)
      ]
