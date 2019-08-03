module BankAccount
    ( BankAccount
    , closeAccount
    , getBalance
    , incrementBalance
    , openAccount
    ,decrementBalance
    ) where

import Control.Concurrent
import Control.Monad (void)

-- The task is to create the data type `BankAccount` and
-- and implement the functions below.

data BankAccount = BankAccount { balance :: MVar (Maybe Integer) }

closeAccount :: BankAccount -> IO ()
closeAccount ba = void $ swapMVar (balance ba) Nothing

getBalance :: BankAccount -> IO (Maybe Integer)
getBalance = readMVar . balance

incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance ba n = modifyMVar_ (balance ba) (return . fmap (+ n)) >> getBalance ba

decrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
decrementBalance ba n = modifyMVar_ (balance ba) (return . fmap (+ (- n))) >> getBalance ba

openAccount :: IO BankAccount
openAccount = BankAccount `fmap` newMVar (Just 0)
