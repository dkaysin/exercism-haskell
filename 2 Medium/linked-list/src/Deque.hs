module Deque (Deque, mkDeque, pop, push, shift, unshift) where

import           Data.IORef

data Deque a = Deque {front :: IORef (Node a), back :: IORef (Node a)}
data Node a = End | Node {value :: a, next :: IORef (Node a), prev :: IORef (Node a)}

mkDeque :: IO (Deque a)
mkDeque = do
  fVal <- newIORef End
  bVal <- newIORef End
  return Deque {front=fVal, back=bVal}

-- Remove from back
pop :: Deque a -> IO (Maybe a)
pop Deque {back=backIO} = do
  lastNode <- readIORef backIO
  case lastNode of
    Node {value=x, next=nextIO} -> do
      newBack <- readIORef nextIO
      writeIORef backIO newBack
      return $ Just x
    End -> return Nothing

-- Remove from front
shift :: Deque a -> IO (Maybe a)
shift Deque {front=frontIO} = do
  firstNode <- readIORef frontIO
  case firstNode of
    Node {value=x, prev=prevIO} -> do
      newFront <- readIORef prevIO
      writeIORef frontIO newFront
      return $ Just x
    End -> return Nothing

-- Add to back
push :: Deque a -> a -> IO ()
push Deque {front=frontIO, back=backIO} x = do
  lastNode <- readIORef backIO
  newNextIO <- newIORef lastNode
  newPrevIO <- newIORef End
  let newNode = Node{value=x, next=newNextIO, prev=newPrevIO}
  case lastNode of
    Node{prev=prevIO} -> do
      writeIORef backIO newNode
      writeIORef prevIO newNode
    End -> do
      writeIORef backIO newNode
      writeIORef frontIO newNode

-- Add to front
unshift :: Deque a -> a -> IO ()
unshift Deque {front=frontIO, back=backIO} x = do
  firstNode <- readIORef frontIO
  newNextIO <- newIORef End
  newPrevIO <- newIORef firstNode
  let newNode = Node{value=x, next=newNextIO, prev=newPrevIO}
  case firstNode of
    Node{next=nextIO} -> do
      writeIORef frontIO newNode
      writeIORef nextIO newNode
    End -> do
      writeIORef backIO newNode
      writeIORef frontIO newNode
