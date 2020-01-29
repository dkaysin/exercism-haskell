module CustomSet
  ( delete
  , difference
  , empty
  , fromList
  , insert
  , intersection
  , isDisjointFrom
  , isSubsetOf
  , member
  , null
  , size
  , toList
  , union
  ) where

import           Prelude hiding (null)

-- Look Mum No Ord!
data CustomSet a = CustomSet [a] deriving (Show)
instance Eq a => Eq (CustomSet a) where
  set1 == set2 = set1 `isSubsetOf` set2 &&  set2 `isSubsetOf` set1

delete :: Eq a => a -> CustomSet a -> CustomSet a
delete x (CustomSet lst) = CustomSet $ filter (/=x) lst

difference :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
difference (CustomSet a) (CustomSet b) = CustomSet $ filter (`notElem` b) a

empty :: CustomSet a
empty = CustomSet []

fromList :: Eq a => [a] -> CustomSet a
fromList = foldr insert empty

insert :: Eq a => a -> CustomSet a -> CustomSet a
insert x (CustomSet xs)
  | x `elem` xs = CustomSet xs
  | otherwise = CustomSet $ x:xs

intersection :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
intersection (CustomSet xa) (CustomSet xb) = CustomSet $ filter (`elem` xb) xa

isDisjointFrom :: Eq a => CustomSet a -> CustomSet a -> Bool
isDisjointFrom setA setB = null $ intersection setA setB

isSubsetOf :: Eq a => CustomSet a -> CustomSet a -> Bool
isSubsetOf setA setB = null $ difference setA setB

member :: Eq a => a -> CustomSet a -> Bool
member x (CustomSet xs) = x `elem` xs

null :: CustomSet a -> Bool
null set = size set == 0

size :: CustomSet a -> Int
size (CustomSet xs) = length xs

toList :: CustomSet a -> [a]
toList (CustomSet xs) = xs

union :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
union (CustomSet xs) targetSet = foldr insert targetSet xs
