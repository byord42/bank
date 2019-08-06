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

-- funcao que retorna um IO vazio
closeAccount :: BankAccount -> IO ()
closeAccount ba = void $ swapMVar (balance ba) Nothing

-- funcao que retorna o valor da variavel
getBalance :: BankAccount -> IO (Maybe Integer)
getBalance = readMVar . balance

-- retorna o valor da conta acrescido do valor passado
incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance ba n = modifyMVar_ (balance ba) (return . fmap (+ n)) >> getBalance ba

-- retorna o valor da conta decrementado
decrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
decrementBalance ba n = modifyMVar_ (balance ba) (return . fmap (+ (- n))) >> getBalance ba

-- retorna um tipo de dado conta
openAccount :: IO BankAccount
openAccount = BankAccount `fmap` newMVar (Just 0)
