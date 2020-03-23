module Triangle (TriangleType(..), triangleType) where

data TriangleType = Equilateral
                  | Isosceles
                  | Scalene
                  | Illegal
                  deriving (Eq, Show)

triangleType :: (Num a, Ord a) => a -> a -> a -> TriangleType
triangleType a b c
  | any (<=0) sides = Illegal
  | (a+b<=c) || (a+c<=b) || (b+c<=a) = Illegal
  | (a==b) && (b==c) = Equilateral
  | (a==b) || (b==c) || (a==c) = Isosceles
  | otherwise = Scalene
  where sides = [a,b,c]
