module BST
    ( BST
    , bstLeft
    , bstRight
    , bstValue
    , empty
    , fromList
    , insert
    , singleton
    , toList
    ) where

data BST a = BST a (BST a) (BST a) | End deriving (Eq, Show)

bstLeft :: BST a -> Maybe (BST a)
bstLeft End            = Nothing
bstLeft (BST _ left _) = Just left

bstRight :: BST a -> Maybe (BST a)
bstRight End             = Nothing
bstRight (BST _ _ right) = Just right

bstValue :: BST a -> Maybe a
bstValue End           = Nothing
bstValue (BST val _ _) = Just val

empty :: BST a
empty = End

fromList :: Ord a => [a] -> BST a
fromList = foldl (flip insert) empty

insert :: Ord a => a -> BST a -> BST a
insert new End = singleton new
insert new (BST val left right)
  | new <= val = BST val (insert new left) right
  | otherwise = BST val left (insert new right)

singleton :: a -> BST a
singleton x = BST x empty empty

toList :: BST a -> [a]
toList End                  = []
toList (BST val left right) = toList left ++ [val] ++ toList right

-- Extra

foldBFS :: (a -> b -> a) -> a -> BST b -> a
foldBFS _ acc End = acc
foldBFS f acc1 (BST val left right) = acc4
  where
    acc2 = f acc1 val
    acc3 = foldBFS f acc2 left
    acc4 = foldBFS f acc3 right

append :: Ord a => BST a -> BST a -> BST a
append branch End  = branch
append branch tree = foldBFS (flip insert) tree branch
