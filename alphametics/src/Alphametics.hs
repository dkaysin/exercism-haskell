module Alphametics (solve) where

import           Control.Monad (foldM, guard)
import           Data.Char     (isAlpha)
import           Data.List
import           Data.Maybe

type Term = String
type Lut = [(Char, Int)]

data Operation = Equal | Sum | Diff | Mult | Div | Term String | Number Int | End
  deriving (Show)
type Stack = ([Int], [Operation])

emptyStack :: Stack
emptyStack = ([],[])

solve :: String -> Maybe [(Char, Int)]
solve = safeHead . listSolutions

listSolutions :: String -> [Lut]
listSolutions xs = do
  let ops = parse $ words xs
  lut <- getLuts ops
  let translated = map (termToNumber lut) ops
  guard $ checkStack $ foldM stepStack emptyStack translated
  return lut

checkStack :: Maybe Stack -> Bool
checkStack (Just (n:_, _)) = n == 1
checkStack _               = False

parse :: [Term] -> [Operation]
parse [] = [End]
parse (t:xt) = word : parse xt
  where
    word
      | t == "==" = Equal
      | t == "+" = Sum
      | t == "-" = Diff
      | t == "*" = Mult
      | t == "/" = Div
      | otherwise = Term t

stepStack :: Stack -> Operation -> Maybe Stack
stepStack st@(terms, actions) op = case op of
  End      -> return st
  Number t -> evalStack (t:terms, actions)
  _        -> return (terms, op:actions)

evalStack :: Stack -> Maybe Stack
evalStack st@([_], _)         = Just st
evalStack (t1:t2:xt, a:xa) = do
  newVal <- operate a t1 t2
  return (newVal : xt, xa)
evalStack _ = Nothing

operate :: Operation -> Int -> Int -> Maybe Int
operate Sum   = binaryOperation (+)
operate Diff  = binaryOperation (-)
operate Mult  = binaryOperation (*)
operate Div   = binaryOperation div
operate Equal = \a b -> if b == a then Just 1 else Just 0
operate _     = \_ _ -> Nothing

binaryOperation :: (Int -> Int -> Int) -> Int -> Int -> Maybe Int
binaryOperation f a b = Just $ f a b

wordToNum :: Lut -> Term -> Maybe Int
wordToNum lut xs = do
  nums <- mapM (`lookup` lut) xs
  return $ read $ concatMap show nums

getLuts :: [Operation] -> [Lut]
getLuts ops = do
  let terms = map termToString $ filter isTerm ops
      chars = nub $ concat terms
      len = length chars
  nums <- permute len [0..9]
  let lut = zip chars nums
  guard $ all (\x -> x `lookup` lut /= Just 0) $ map head terms
  return lut

-- Helper functions
permute :: Eq a => Int -> [a] -> [[a]]
permute 0 _ = [[]]
permute n xa = do
  a <- xa
  b <- permute (n-1) (delete a xa)
  return $ a : b

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (a:_) = Just a

isTerm :: Operation -> Bool
isTerm (Term _) = True
isTerm _        = False

termToString :: Operation -> String
termToString (Term t) = t
termToString _        = ""

termToNumber :: Lut -> Operation -> Operation
termToNumber lut (Term t) = Number $ fromMaybe 0 (wordToNum lut t)
termToNumber _ op         = op
