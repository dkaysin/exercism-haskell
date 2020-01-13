module Squares (difference, squareOfSum, sumOfSquares) where

difference :: Integral a => a -> a
difference n = squareOfSum n - sumOfSquares n

squareOfSum :: Integral a => a -> a
squareOfSum = (^2) . sum . first

sumOfSquares :: Integral a => a -> a
sumOfSquares = sum . map (^2) . first

first :: Integral a => a -> [a]
first n = [1..n]
