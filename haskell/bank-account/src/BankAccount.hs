module BankAccount
    ( BankAccount
    , closeAccount
    , getBalance
    , incrementBalance
    , openAccount
    ) where

import Control.Concurrent
import System.IO.Unsafe

data BankAccount = BankAccount { balance :: MVar Integer,
                                 active  :: MVar Bool
                               }

closeAccount :: BankAccount -> IO ()
closeAccount account = do
  takeMVar $ active account
  putMVar (active account) False

getBalance :: BankAccount -> IO (Maybe Integer)
getBalance account = do
  active <- readMVar $ active account
  if active
    then fmap Just $ readMVar $ balance account
    else return Nothing

incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance account amount = do
  currentBalance <- takeMVar $ balance account
  putMVar (balance account) (currentBalance + amount)
  getBalance account

openAccount :: IO BankAccount
openAccount = do
  balance <- newMVar 0
  active <- newMVar True
  return BankAccount { balance=balance, active=active }

