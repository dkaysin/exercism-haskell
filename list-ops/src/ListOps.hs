module ListOps
  ( length
  , reverse
  , map
  , filter
  , foldr
  , foldl'
  , (++)
  , concat
  ) where

-- We assume that (:) is already implemented
import           Prelude hiding (concat, filter, foldl, foldl', foldr, length,
                          map, reverse, (++))

-- Non-optimized version of foldl with non-strict accumulator                    --
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ z []     = z
foldl f z (x:xs) = foldl f (f z x) xs

-- Version of foldl with strict accumulator
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ z []     = z
foldl' f z (x:xs) = seq z0 $ foldl' f z0 xs
  where z0 = f z x

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ z []     = z
foldr f z (x:xs) = f x (foldr f z xs)

length :: [a] -> Int
length = foldr (\_ acc -> acc + 1) 0

reverse :: [a] -> [a]
reverse = foldl' (flip (:)) []

map :: (a -> b) -> [a] -> [b]
map f = foldr ((:).f) []

filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr (\ x acc -> if p x then x:acc else acc) []

(++) :: [a] -> [a] -> [a]
xs ++ ys = foldr (:) ys xs

concat :: [[a]] -> [a]
concat = foldr (++) []
