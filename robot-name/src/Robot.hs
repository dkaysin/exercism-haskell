module Robot (Robot, initialState, mkRobot, resetName, robotName) where

import           Control.Monad.State
import           Data.List
import           System.Random

data Robot = Robot{name :: String} deriving (Show)
data RunState = RunState{allNames :: [String]}

initialState :: RunState
initialState = RunState{allNames = []}

mkRobot :: StateT RunState IO Robot
mkRobot = do
  st <- get
  newName <- liftIO $ genUniqueName $ allNames st
  put RunState{allNames = newName : allNames st}
  return Robot{name = newName}

-- Returned value should be used
resetName :: Robot -> StateT RunState IO Robot
resetName Robot{name = oldName} = do
  st <- get
  newName <- liftIO $ genUniqueName $ allNames st
  put RunState{allNames = newName : delete oldName (allNames st)}
  return Robot{name = newName}

robotName :: Robot -> IO String
robotName robot = return $ name robot

genUniqueName :: [String] -> IO String
genUniqueName names = do
  newName <- randName
  if newName `notElem` names
    then return newName
    else genUniqueName names

randName :: IO String
randName = do
    letters <- replicateM 2 $ getStdRandom (randomR ('A', 'Z'))
    numbers <- getStdRandom (randomR (100::Int, 999))
    return $ letters ++ show numbers
