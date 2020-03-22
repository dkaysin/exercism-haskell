{-# LANGUAGE OverloadedStrings #-}

module Forth
  ( ForthError(..)
  , ForthState
  , evalText
  , toList
  , emptyState
  ) where

import           Control.Monad
import           Data.List
import           Data.Map       (Map)
import qualified Data.Map       as M
import           Data.Text      (Text)
import qualified Data.Text      as T
import           Data.Text.Read (decimal)

data ForthError
     = DivisionByZero
     | StackUnderflow
     | InvalidWord
     | UnknownWord Text
     deriving (Show, Eq)

type StackManip = ForthState -> Either ForthError ForthState
type Dict = Map Text StackManip
type Stack = [Int]
data ForthState = ForthState { stack :: Stack, dict :: Dict }

simpleShow :: ForthState -> String
simpleShow ForthState { stack = st } = show st

emptyState :: ForthState
emptyState = ForthState [] $ M.fromList [
    ("dup", dup)
  , ("drop", drop')
  , ("swap", swap)
  , ("over", over)
  , ("+", genericBinOp (+))
  , ("-", genericBinOp (-))
  , ("*", genericBinOp (*))
  , ("/", div')
  , (";", return)
  ]

evalText :: Text -> StackManip
evalText txt = parseLine $ T.words txt

toList :: ForthState -> [Int]
toList (ForthState xa _) = reverse xa

-- Parsing into StackManip type (i.e., into function operating on a stack)

parseLine :: [Text] -> StackManip
parseLine [] = return
parseLine (":" : name : xw) = \ state -> case decimal name of
    (Left _) -> let f = composeSM $ map (parseWordVal state) xw
      in Right state{ dict = M.insert (T.toLower name) f (dict state) }
    (Right _) -> Left InvalidWord
parseLine xw = composeSM $ map parseWordRef xw

-- Difference between the following two implementations of parseWordXXX
-- becomes apparent for the following sequence of instructions:
--    ": foo 5 ;"
--  , ": bar foo ;"
--  , ": foo 6 ;"
--  , "bar foo"
-- When "parseWordVal" function is used in "parseLine" above, the output is:
-- [5,6], i.e. definition of "bar" becomes fixed and no longer changes with changes of "foo"
-- However when "parseWordRef" function is used instead, the output is:
-- [6,6], i.e. definition of "bar" uses the updated definition of "foo" under the hood

-- Custom words within the line are parsed via references to the latest state
parseWordRef :: Text -> StackManip
parseWordRef txt = case decimal txt of
  (Left _)      -> \ state -> case T.toLower txt `M.lookup` dict state of
    (Just f) -> f state
    Nothing  -> Left $ UnknownWord txt
  (Right (n,_)) -> put n

-- Custom words are dereferenced using the state provided
parseWordVal :: ForthState -> Text -> StackManip
parseWordVal state txt = case decimal txt of
  (Left _)      -> case T.toLower txt `M.lookup` dict state of
    (Just f) -> f
    Nothing  -> \ _ -> Left $ UnknownWord txt
  (Right (n,_)) -> put n

composeSM :: [StackManip] -> StackManip
composeSM = foldl' (>=>) return

-- Raw manipulations of the stack
-- (building blocks for more complex stack manipulations)

put :: Int -> StackManip
put a st@ForthState{ stack = xa } = Right $ st{ stack = a:xa }

dup :: StackManip
dup st@ForthState{ stack = (a:xa) } = Right $ st{ stack = a:a:xa }
dup _                               = Left StackUnderflow

drop' :: StackManip
drop' st@ForthState{ stack = (_:xa) } = Right $ st{ stack = xa }
drop' _                               = Left StackUnderflow

swap :: StackManip
swap st@ForthState{ stack = (a:b:xa) } = Right $ st{ stack = b:a:xa }
swap _                                 = Left StackUnderflow

over :: StackManip
over st@ForthState{ stack = (a:b:xa) } = Right $ st{ stack = b:a:b:xa }
over _                                 = Left StackUnderflow

genericBinOp :: (Int -> Int -> Int) -> StackManip
genericBinOp f st@ForthState{ stack = (a:b:xa) } = Right $ st{ stack = b `f` a : xa }
genericBinOp _ _                     = Left StackUnderflow

div' :: StackManip
div' ForthState{ stack = (0:_:_) }     = Left DivisionByZero
div' st@ForthState{ stack = (a:b:xa) } = Right $ st{ stack =  b `div` a : xa }
div' _                                 = Left StackUnderflow
