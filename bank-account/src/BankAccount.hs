module BankAccount
    ( BankAccount
    , closeAccount
    , getBalance
    , incrementBalance
    , openAccount
    ) where

import           Control.Concurrent.STM
import           Control.Monad          (when)

data BankAccount = BankAccount{ amount :: TVar (Maybe Integer) }

closeAccount :: BankAccount -> IO ()
closeAccount BankAccount{ amount = a } =
  atomically $ writeTVar a Nothing

getBalance :: BankAccount -> IO (Maybe Integer)
getBalance BankAccount{ amount = a } =
  atomically $ readTVar a

incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance BankAccount{ amount = a } incrVal = do
  atomically (do
      amountVal <- readTVar a
      let newAmountVal = (+incrVal) <$> amountVal
      when (all (>=0) newAmountVal) $ writeTVar a newAmountVal
    )
  atomically $ readTVar a

openAccount :: IO BankAccount
openAccount = do
  a <- newTVarIO $ Just 0
  return BankAccount{ amount = a }
