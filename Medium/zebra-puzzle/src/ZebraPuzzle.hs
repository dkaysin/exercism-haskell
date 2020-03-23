module ZebraPuzzle (Resident(..), Solution(..), solve) where

import           Control.Monad
import           Data.List
import           Data.Map      (Map)
import qualified Data.Map      as M

data Resident = Englishman | Spaniard | Ukrainian | Norwegian | Japanese deriving (Eq, Ord, Enum, Show)
data Pet = Dog | Snail | Fox | Horse | Zebra deriving (Eq, Ord, Enum, Show)
data Drink = Coffee | Tea | OrangeJuice | Milk | Water deriving (Eq, Ord, Enum, Show)
data Smoke = OldGold | Parliaments | Chesterfields | LuckyStrike | Kools deriving (Eq, Ord, Enum, Show)
data Color = Blue | Red | Ivory | Yellow | Green deriving (Eq, Ord, Enum, Show)

type OrdList a = Map a Int

type Option = (OrdList Resident, OrdList Pet, OrdList Drink, OrdList Smoke, OrdList Color)
data Solution = Solution { waterDrinker :: Resident
                         , zebraOwner   :: Resident } deriving (Eq, Show)

solve :: Solution
solve = let (resident, pet, drink, _, _) = head findOptions
            getResident (x, xM) = M.fromList (map (\(a,b)->(b,a)) $ M.assocs resident) M.! (xM M.! x)
  in Solution {
      waterDrinker = getResident (Water, drink)
    , zebraOwner = getResident (Zebra, pet)
    }

-- Order of constraints optimized for efficiency
findOptions ::[Option]
findOptions = do
  resident <- permuteM [Englishman ..]
  guard $ (Norwegian, resident) `at` 1
  color <- permuteM [Blue ..]
  guard $ (Englishman, resident) `is` (Red, color)
  guard $ (Green, color) `toTheRight` (Ivory, color)
  guard $ (Norwegian, resident) `near` (Blue, color)
  drink <- permuteM [Coffee ..]
  guard $ (Milk, drink) `at` 3
  guard $ (Coffee, drink) `is` (Green, color)
  guard $ (Tea, drink) `is` (Ukrainian, resident)
  pet <- permuteM [Dog ..]
  guard $ (Spaniard, resident) `is` (Dog, pet)
  smoke <- permuteM [OldGold ..]
  guard $ (OldGold, smoke) `is` (Snail, pet)
  guard $ (Kools, smoke) `is` (Yellow, color)
  guard $ (Chesterfields, smoke) `near` (Fox, pet)
  guard $ (Kools, smoke) `near` (Horse, pet)
  guard $ (LuckyStrike, smoke) `is` (OrangeJuice, drink)
  guard $ (Japanese, resident) `is` (Parliaments, smoke)
  return (resident, pet, drink, smoke, color)

-- Predicates

near :: (Ord a, Ord b) => (a, OrdList a) -> (b, OrdList b) -> Bool
near (a, ma) (b, mb) = abs (ia - ib) == 1
  && ia >= 1 && ia <= 5 && ib >= 1 && ib <= 5
  where
    ia = ma M.! a
    ib = mb M.! b

toTheRight :: (Ord a, Ord b) => (a, OrdList a) -> (b, OrdList b) -> Bool
toTheRight (a, ma) (b, mb) = ia - ib == 1
  && ia >= 1 && ia <= 5 && ib >= 1 && ib <= 5
  where
    ia = ma M.! a
    ib = mb M.! b

is :: (Ord a, Ord b) => (a, OrdList a) -> (b, OrdList b) -> Bool
is (a, ma) (b, mb) = ia == ib
  where
    ia = ma M.! a
    ib = mb M.! b

at :: (Ord a) => (a, OrdList a) -> Int -> Bool
at (a, ma) n = ia == n
  where
    ia = ma M.! a

-- Getting permutations

permuteM :: (Ord a) => [a] -> [Map a Int]
permuteM xn = map (\p -> M.fromList $ zip p [1..]) $ permute xn

permute :: Eq a => [a] -> [[a]]
permute [] = [[]]
permute xa = do
  a <- xa
  rest <- permute $ delete a xa
  return $ a : rest
