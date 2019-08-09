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
        putStrLn ("Olá, qual é o seu nome?")
        -- get input from user
        userInput <- getLine
        putStrLn ("Olá " ++ userInput ++ " , bem vindo ao Banco Carlo Kleber LTDA")
        account <- openAccount
        putStrLn ("Agora sua conta foi criada! Você tem um total de 0 reais.")
    
        -- loop using recursion
        --  but do notation is merely syntactic sugar for nested applications of (>>=), which is itself nothing more than an infix higher-order function
        -- http://www.haskellforall.com/2013/07/statements-vs-expressions.html
        let loop = do
            -- echo back to the user
            putStrLn("Que ação você deseja realizar?")
            putStrLn("1 - Depositar")
            putStrLn("2 - Sacar")
            putStrLn("3 - Projetar o rendimento do seu dinheiro")
            putStrLn("4 - Sair")
            userInput <- getLine
            case userInput of
                "1" -> do
                    --nested expressions, and sequencing statements just builds larger and larger expressions
                        putStrLn("Quanto você quer depositar?")
                        -- get input from user
                        value <- getLine
                        let depositValue = (read :: String -> Integer) value
                        balance <- incrementBalance account depositValue
                        putStrLn("Seu depósito foi realizado. Seu saldo atual é de " ++balance++ " reais.")
                "2" -> do
                        putStrLn("Quanto você quer sacar? Você pode sacar no máximo " ++ balance ++ " reais.")
                        value <- getLine
                        let withdrawValue = (read :: String -> Integer) value
                        balance <- decrementBalance account withdrawValue
                        putStrLn("Seu saque foi realizado. Seu saldo atual é de " ++balance++ " reais.")
                "3" -> do
                        balance <- getBalance account
                        putStrln("Seu saldo atual é de " ++ balance ++ " reais. Você gostaria de fazer a projeção de rendimento para quantos dias?")
                        value <- getLine
                        let days = (read :: String -> Integer) value
                        let rendimento = (\x -> 1 + x) 0.02 --lambda termo
                        rendimentoFinal :: Integer -> Integer
                        let rendimentoFinal = (balance*(rendimento^days))
                        putStrln("Daqui a " ++ days ++ " dias o seu saldo será de " ++ rendimentoFinal ++ " reais se você não realizar nenhuma movimentação na conta.")
                "4" -> putStrLn("Até mais!")
                
            -- putStrLn ("Hey " ++ userInput ++ ", you rock!")
            -- putStrLn("How much do you have?")
            
            --account <- openAccount
            --- putStrLn("How much do you have?")
            ---balance <- getBalance account
            -- putStrLn ("type of action2 is: " ++ (show (typeOf balance)))
            ---print balance
            --balance <- incrementBalance account 10
            --Just balance -> putStrLn $ "Value: " ++ show balance
            --case balance of
                --Just a -> putStrLn $ "Your balance is :"++show a++"."
            -- quando o userInput for q ele sai do loop
        when (userInput /= "4") loop
     
        loop  -- start the first iteration
    
    
    
