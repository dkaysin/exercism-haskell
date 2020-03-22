{-# LANGUAGE OverloadedStrings #-}

module Sgf (parseSgf) where

import           Data.Map         (Map)
import qualified Data.Map         as M
import           Data.Text        (Text)
import qualified Data.Text        as T
import           Data.Tree

import           Text.Parsec
import           Text.Parsec.Text

type KeyValue = (Text, [Text])
type SgfNode = Map Text [Text]
type SgfTree = Tree SgfNode

parseSgf :: Text -> Maybe SgfTree
parseSgf sgf = case parse tree "Parse failed!" sgf of
  Left _  -> Nothing
  Right t -> Just t

-- Parse KeyValues

key :: Parser Text
key = T.pack <$> many1 upper

value :: Parser Text
value = do
  char '['
  v <- many1 $ charWhitespace
    <|> try escapedCharAllow <|> try escapedCharIgnore
    <|> many1 alphaNum
  char ']'
  return $ T.pack $ concat v

escapedCharAllow :: Parser String
escapedCharAllow = do
  char '\\'
  c <- char ']' <|> char '[' <|> char '\\'
  return [c]

escapedCharIgnore :: Parser String
escapedCharIgnore = do
  char '\\'
  anyChar
  return ""

charWhitespace :: Parser String
charWhitespace = do
  space
  return " "

keyvalue :: Parser KeyValue
keyvalue = do
  k <- key
  v <- many1 value
  return (k, v)

-- Parse nodes and trees

node :: Parser SgfNode
node = do
  char ';'
  nodes <- many keyvalue
  return $ M.fromList nodes

tree :: Parser SgfTree
tree = do
  char '('
  (Node n xt) <- chainNodes <$> many1 node
  children <- many tree
  char ')'
  return $ Node n (xt ++ children)

chainNodes :: [SgfNode] -> SgfTree
chainNodes [n]    = Node n []
chainNodes (n:xn) = Node n [chainNodes xn]
