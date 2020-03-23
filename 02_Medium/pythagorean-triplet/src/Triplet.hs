module Triplet (tripletsWithSum) where

tripletsWithSum :: Int -> [(Int,Int,Int)]
tripletsWithSum n =
    filter (\(a,b,c) -> a*a+b*b == c*c)
    $ triplets n

triplets :: Int -> [(Int,Int,Int)]
triplets n = [
  (a, b, c)
    | a <- [1..n`div`3]
    , b <-[a..(n-a)`div`2]
    , c <- [n-a-b]
  ]
