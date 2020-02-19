module WordProblem (answer) where

import           Control.Monad  (foldM)
import           Data.Maybe
import           Data.Text      (Text)
import qualified Data.Text      as T
import           Data.Text.Read

data Operation = Const | Sum | Diff | Mult | Div | Exp | Number Integer | End
  deriving (Eq, Show)

type Stack = ([Integer], [Operation])
emptyStack :: Stack
emptyStack = ([], [])

answer :: String -> Maybe Integer
answer xs = do
  ops <- sequence $ parse $ split xs
  (res:_, _) <- foldM stepStack emptyStack ops
  return res

stepStack :: Stack -> Operation -> Maybe Stack
stepStack st@(nums, actions) op = case op of
  End      -> if null actions then Just st else Nothing
  Const    -> Just st
  Number n -> evalStack (n:nums, actions)
  _        -> Just (nums, op:actions)

evalStack :: Stack -> Maybe Stack
evalStack st@(nums, actions)
  | la == 0 && ln < 2 = Just st
  | ln == 2 && la == 1 = do
    let (a:xa) = actions
    newNums <- operate a nums
    return (newNums, xa)
  | otherwise = Nothing
  where
    ln = length nums
    la = length actions

operate :: Operation -> [Integer] -> Maybe [Integer]
operate Sum  = binaryOperate (+)
operate Diff = binaryOperate (-)
operate Mult = binaryOperate (*)
operate Div  = binaryOperate div
operate Exp  = binaryOperate (^)
operate _    = const Nothing

binaryOperate :: (Integer -> Integer -> Integer) -> [Integer] -> Maybe [Integer]
binaryOperate f [a,b] = Just [f b a]
binaryOperate _ _     = Nothing

split :: String -> [Text]
split = map (T.filter isAllowedSymbol) . T.words . T.pack

parse :: [Text] -> [Maybe Operation]
parse [] = [Just End]
parse line = fst res : parse (snd res)
  where res = parse1 line

-- Parsing functions
parse1 :: [Text] -> (Maybe Operation, [Text])
parse1 line@(x1:rest)
  | isJust tryNumber = (Number . fst <$> tryNumber, rest)
  | prefix == T.pack "plus" = (Just Sum, rest)
  | prefix == T.pack "minus" = (Just Diff, rest)
  | prefix == T.pack "power" = (Just Const, rest)
  | otherwise = parse2 line
  where
    prefix = fromMaybe x1 (T.stripSuffix (T.pack "th") x1)
    tryNumber = eitherToMaybe (signed decimal prefix)
parse1 _ = (Nothing, [])

parse2 :: [Text] -> (Maybe Operation, [Text])
parse2 line@(x1:x2:rest)
  | prefix == T.pack "multiplied by" = (Just Mult, rest)
  | prefix == T.pack "divided by" = (Just Div, rest)
  | prefix == T.pack "What is" = (Just Const, rest)
  | otherwise = parse3 line
  where prefix = T.unwords [x1,x2]
parse2 _ = (Nothing, [])

parse3 :: [Text] -> (Maybe Operation, [Text])
parse3 (x1:x2:x3:rest)
  | prefix == T.pack "raised to the" = (Just Exp, rest)
  | otherwise = (Nothing, [])
  where prefix = T.unwords [x1,x2,x3]
parse3 _ = (Nothing, [])

-- Helper functions
eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _)  = Nothing
eitherToMaybe (Right b) = Just b

isAllowedSymbol :: Char -> Bool
isAllowedSymbol s = s `elem` dict
  where dict = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['-']
