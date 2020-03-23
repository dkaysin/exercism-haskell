module Sieve (primesUpTo) where

-- You should not use any of the division operations when implementing
-- the sieve of Eratosthenes.
import           Prelude hiding (div, divMod, mod, quot, quotRem, rem, (/))

data Zipper a = Zipper [a] [a]

primesUpTo :: Integer -> [Integer]
primesUpTo n = reverse left
  where (Zipper left _) = go $ Zipper [] [2..n]

go :: Zipper Integer -> Zipper Integer
go zp@(Zipper _ []) = zp
go (Zipper left (n:xn)) = go $ Zipper nextleft nextright
  where
    nextleft = n:left
    nextright = filter (not.(`elemOrd` mults)) xn
    mults = (*n) <$> [2..]

elemOrd :: Ord a => a -> [a] -> Bool
elemOrd _ [] = False
elemOrd k (x:xn) = case compare x k of
  EQ -> True
  GT -> False
  LT -> elemOrd k xn
