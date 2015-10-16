{-# LANGUAGE TemplateHaskell #-}
module BankAccount
  ( BankAccount
  , openAccount
  , closeAccount
  , getBalance
  , incrementBalance
  ) where

import Data.IORef
import Control.Lens

type BankAccount = IORef BankAccountFields

data BankAccountFields = BAF
  { _baBalance :: Int
  , _baIsOpen :: Bool
  }

makeLenses ''BankAccountFields

openAccount :: IO BankAccount
openAccount = newIORef (BAF 0 True)

closeAccount :: BankAccount -> IO ()
closeAccount ba = modifyIORef' ba (& baIsOpen .~ False)

getBalance :: BankAccount -> IO (Maybe Int)
getBalance x = do
    BAF { _baBalance = b
        , _baIsOpen = o
        } <- readIORef x
    return $
        if o then Just b
             else Nothing

incrementBalance :: BankAccount -> Int -> IO (Maybe Int)
incrementBalance x v = do
    BAF { _baBalance = b
        , _baIsOpen = o
        } <- readIORef x
    if o
      then do
        modifyIORef' x (& baBalance .~ (b+v))
        return (Just (b+v))
      else return Nothing
