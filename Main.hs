module Main where
    import BankAccount( closeAccount
        , incrementBalance
        , decrementBalance
        , getBalance
        , openAccount
        )
    import Lib
    import Data.Typeable
    import Control.Monad
    
    
    main :: IO ()
    
    main = do  
        putStrLn "Hello, what's your name?"
        -- get input from user
        userInput <- getLine
        putStrLn ("Hello " ++ userInput ++ ", welcome to Functional Bank LTDA")
        account <- openAccount
        putStrLn ("Now that we created your account you can do those actions")
    
        -- loop using recursion
        --  but do notation is merely syntactic sugar for nested applications of (>>=), which is itself nothing more than an infix higher-order function
        -- http://www.haskellforall.com/2013/07/statements-vs-expressions.html
        let loop = do
            -- echo back to the user
            putStrLn("1 - Deposit")
            putStrLn("2 - Withdraw")
            putStrLn("q - Quit!")
            userInput <- getLine
            case userInput of
                "1" -> do
                    --nested expressions, and sequencing statements just builds larger and larger expressions
                        putStrLn("How much ?")
                        -- get input from user
                        value <- getLine
                        let depositValue = (read :: String -> Integer) value
                        balance <- incrementBalance account depositValue
                        putStrLn("Your deposit has been made")
                "2" -> do
                        putStrLn("How much ?")
                        value <- getLine
                        let witdrawValue = (read :: String -> Integer) value
                        balance <- decrementBalance account witdrawValue
                        putStrLn("Your witdraw has been made")
                "q" -> putStrLn("Good Bye !")
                
            -- putStrLn ("Hey " ++ userInput ++ ", you rock!")
            -- putStrLn("How much do you have?")
            
            --account <- openAccount
            --- putStrLn("How much do you have?")
            balance <- getBalance account
            -- putStrLn ("type of action2 is: " ++ (show (typeOf balance)))
            print balance
            --balance <- incrementBalance account 10
            --Just balance -> putStrLn $ "Value: " ++ show balance
            case balance of
                Just a -> putStrLn $ "Your balance is :"++show a++"."
            -- quando o userInput for q ele sai do loop
            when (userInput /= "q") loop
        loop  -- start the first iteration 
        putStrLn "Good bye!"
    
    
    