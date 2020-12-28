module BankAccount
  ( BankAccount
  , openAccount
  , closeAccount
  , getBalance
  , incrementBalance
  ) where

import Data.IORef

type BankAccount = IORef BankAccountFields

data BankAccountFields = BAF
  { _baBalance :: Int
  , _baIsOpen :: Bool
  }

openAccount :: IO BankAccount
openAccount = newIORef (BAF 0 True)

closeAccount :: BankAccount -> IO ()
closeAccount ba = atomicModifyIORef' ba closeAccount'
  where
    closeAccount' (BAF b o) = if o
      then (BAF b False, ())
      else error "closing closed account"

getBalance :: BankAccount -> IO (Maybe Int)
getBalance x = do
    BAF b o <- readIORef x
    return $
        if o then Just b
             else Nothing

incrementBalance :: BankAccount -> Int -> IO (Maybe Int)
incrementBalance x v = atomicModifyIORef' x incrementBalance'
  where
    incrementBalance' ba@(BAF b o) = if o
      then let newB = b+v in (BAF newB o, Just newB)
      else (ba, Nothing)
