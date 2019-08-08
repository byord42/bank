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
-- http://www.haskellforall.com/2013/07/statements-vs-expressions.html
.
-- the Maybe type (Haskell's version of nullable)

-- An MVar t is mutable location that is either empty or contains a value of type t.
-- constructor of data type
data BankAccount = BankAccount { balance :: MVar (Maybe Integer) }

closeAccount :: BankAccount -> IO ()
closeAccount ba = void $ swapMVar (balance ba) Nothing

getBalance :: BankAccount -> IO (Maybe Integer)
getBalance = readMVar . balance

-- modifyMVar_ :: MVar a -> (a -> IO a) -> IO () 
incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance ba n = modifyMVar_ (balance ba) (return . fmap (+ n)) >> getBalance ba

-- (return . fmap (+ (- n))) = function composition
-- descendingSort = reverse . sort
-- balance ba = accessing value of balance in the BankAccount passed in function 
decrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
decrementBalance ba n = modifyMVar_ (balance ba) (return . fmap (+ (- n))) >> getBalance ba

-- map the value 0 to MVAR balance using 
-- fmap returns a Functor applicated to the (Just 0)
openAccount :: IO BankAccount
openAccount = BankAccount `fmap` newMVar (Just 0)