module Yacht (yacht, Category(..)) where

data Category = Ones
              | Twos
              | Threes
              | Fours
              | Fives
              | Sixes
              | FullHouse
              | FourOfAKind
              | LittleStraight
              | BigStraight
              | Choice
              | Yacht

yacht :: Category -> [Int] -> Int
yacht Ones dice      = (*1) . count 1 $ dice
yacht Twos dice      = (*2) . count 2 $ dice
yacht Threes dice    = (*3) . count 3 $ dice
yacht Fours dice     = (*4) . count 4 $ dice
yacht Fives dice     = (*5) . count 5 $ dice
yacht Sixes dice     = (*6) . count 6 $ dice

yacht FullHouse dice
  | 3 `elem` cs && 2 `elem` cs = sum dice
  | otherwise = 0
  where
    cs = map (`count` dice) [1..6]

yacht FourOfAKind dice
  | any (`elem` cs) [4, 5] = (*4) . sum $ [d | (s, d) <- zip cs [1..6], s >= 4]
  | otherwise = 0
  where
    cs = map (`count` dice) [1..6]

yacht LittleStraight dice
  | all (`elem` dice) [1..5] = 30
  | otherwise = 0

yacht BigStraight dice
  | all (`elem` dice) [2..6] = 30
  | otherwise = 0

yacht Choice dice = sum dice

yacht Yacht dice
  | 5 `elem` cs = 50
  | otherwise = 0
  where
    cs = map (`count` dice) [1..6]

count :: Int -> [Int] -> Int
count n xn = length $ filter (==n) xn
