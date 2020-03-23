module School (School, add, empty, grade, sorted) where

import           Data.List
import qualified Data.Map  as M

data School = School (M.Map Int [String]) deriving (Show)

add :: Int -> String -> School -> School
add gradeNum student (School m) =
  School $ M.insertWith (++) gradeNum [student] m

empty :: School
empty = School M.empty

grade :: Int -> School -> [String]
grade gradeNum (School m) = if M.member gradeNum m
  then sort $ m M.! gradeNum
  else []

sorted :: School -> [(Int, [String])]
sorted (School m) = M.elems $ M.mapWithKey (\ k v -> (k, sort v)) m
