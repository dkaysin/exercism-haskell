module DND ( Character(..)
           , ability
           , modifier
           , character
           ) where

import           Control.Monad   (replicateM)
import           Data.List       (sort)
import           Test.QuickCheck

data Character = Character
  { strength     :: Int
  , dexterity    :: Int
  , constitution :: Int
  , intelligence :: Int
  , wisdom       :: Int
  , charisma     :: Int
  , hitpoints    :: Int
  }
  deriving (Show, Eq)

modifier :: Int -> Int
modifier n = (n-10) `div` 2

ability :: Gen Int
ability = (sum.tail.sort) <$> replicateM 4 dice

dice :: Gen Int
dice = choose (1, 6)

character :: Gen Character
character = do
  str <- ability
  dex <- ability
  con <- ability
  int <- ability
  wis <- ability
  cha <- ability
  return Character {
      strength = str
    , dexterity = dex
    , constitution = con
    , intelligence = int
    , wisdom = wis
    , charisma = cha
    , hitpoints = 10 + modifier con
  }
