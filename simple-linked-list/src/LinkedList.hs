module LinkedList
    ( LinkedList
    , datum
    , fromList
    , isNil
    , new
    , next
    , nil
    , reverseLinkedList
    , toList
    ) where

data LinkedList a = LinkedList a (LinkedList a) | End
  deriving (Eq, Show)

datum :: LinkedList a -> a
datum (LinkedList c _) = c
datum End              = error "Empty element"

fromList :: [a] -> LinkedList a
fromList = foldr new nil

isNil :: LinkedList a -> Bool
isNil (LinkedList _ _) = False
isNil End              = True

new :: a -> LinkedList a -> LinkedList a
new = LinkedList

next :: LinkedList a -> LinkedList a
next (LinkedList _ nxt) = nxt
next End                = nil

nil :: LinkedList a
nil = End

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList lList = switch lList nil

switch :: LinkedList a -> LinkedList a -> LinkedList a
switch (LinkedList c fnxt) second = switch fnxt (new c second)
switch End second                 = second

toList :: LinkedList a -> [a]
toList (LinkedList c nxt) = c : toList nxt
toList End                = []
