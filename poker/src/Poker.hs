module Poker (bestHands) where

import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Data.Function
import           Data.List
import           Data.Maybe
import           Text.ParserCombinators.ReadP

data Face = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Eq, Ord, Enum, Show)

data Color = Club | Spade | Heart | Diamond
  deriving (Eq, Show)
instance Ord Color where
  _ `compare` _ = EQ

newtype Card = Card (Face, Color) deriving (Eq, Ord)
instance Show Card where
  show (Card (face, color)) = let
    swapLookup k dict = k `lookup` map swap dict
    in fromMaybe "Error" (do
      f <- face `swapLookup` faceDict
      c <- color `swapLookup` colorDict
      return $ f++c)

newtype Hand = Hand [Card] deriving (Eq, Ord)
instance Show Hand where
  show (Hand cards) = unwords $ map show cards

data Comb = High | OnePair | TwoPair | ThreeOAK | JuniorStraight | Straight | Flush | FullHouse | FourOAK | SFlash
  deriving (Eq, Ord, Enum, Show, Bounded)

type Tag = (Comb, [Card])

-- Main functions
bestHands :: [String] -> Maybe [String]
bestHands xxs = do
  allHands <- mapM parseHand xxs
  let tags = map getTags allHands
      maxHands = map snd $ filter (\(t,_) -> t `isMaximumIn` tags) $ zip tags allHands
  return $ map show maxHands

getTags :: Hand -> [Tag]
getTags (Hand cards) = do
  tag <- filter (checkComb sortedCards) $ reverse [minBound .. ]
  return (tag, sortedCards)
  where
    groupOn f = groupBy (\(Card a) (Card b) -> f a == f b)
    sortedCards = concat $ sortBy (flip compare `on` length) $ groupOn fst $ sortBy (flip compare) cards

checkComb :: [Card] -> Comb -> Bool
checkComb cards comb = case comb of
  High    -> True
  OnePair -> [2] `isInfixOf` lensFace
  TwoPair -> [2,2] `isInfixOf` lensFace
  ThreeOAK -> [3] `isInfixOf` lensFace
  JuniorStraight -> [Ace, Five, Four, Three, Two] `isInfixOf` faces
  Straight -> [-1,-1,-1,-1] `isInfixOf` diffsFace
  Flush -> [5] `isInfixOf` lensColor
  FullHouse -> [3] `isInfixOf` lensFace && [2] `isInfixOf` lensFace
  FourOAK -> [4] `isInfixOf` lensFace
  SFlash -> [-1,-1,-1,-1] `isInfixOf` diffsFace && [5] `isInfixOf` lensColor
  _       -> False
  where
    faces = map (\(Card x) -> fst x) cards
    colors = map (\(Card x) -> snd x) cards
    lensFace = map length $ group faces
    lensColor = map length $ group colors
    diffsFace = let xs = map fromEnum faces
      in zipWith (-) (tail xs) xs

-- Parsers
parseHand :: String -> Maybe Hand
parseHand xs
  | length res /= 5 = Nothing
  | otherwise = Hand <$> sequence res
  where
    res = do
      parses <- map (readP_to_S cardParser) $ words xs
      return $ join $ safeHead $ map fst parses

cardParser :: ReadP (Maybe Card)
cardParser = do
  faceStr <- count 2 digit <|> count 1 digit <|> count 1 alpha
  colorStr <- count 1 alpha
  eof
  return $ fmap Card $ liftM2 (\a b -> (a,b)) (faceStr `lookup` faceDict) (colorStr `lookup` colorDict)
  where
    digit = satisfy isDigit
    alpha = satisfy isAlpha

-- Helper functions
safeHead :: [a] -> Maybe a
safeHead (a:_) = Just a
safeHead _     = Nothing

swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)

isMaximumIn :: Ord a => a -> [a] -> Bool
isMaximumIn a = all (a>=)

-- Data

faceDict = [
      ("2", Two)
    , ("3", Three)
    , ("4", Four)
    , ("5", Five)
    , ("6", Six)
    , ("7", Seven)
    , ("9", Nine)
    , ("8", Eight)
    , ("10", Ten)
    , ("J", Jack)
    , ("Q", Queen)
    , ("K", King)
    , ("A", Ace)
  ]

colorDict = [
      ("C", Club)
    , ("H", Heart)
    , ("S", Spade)
    , ("D", Diamond)
  ]
