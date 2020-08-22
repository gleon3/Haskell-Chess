module Shell
    ( startShell, printErrorMessage, stringToUpper
    ) where

import Data.Char    
    
error_msg = "Error! "    
    
startShell :: IO ()
startShell = do 
    putStrLn "Welcome!"

printErrorMessage :: String -> IO ()
printErrorMessage msg = putStrLn (error_msg ++ msg)

stringToUpper :: String -> String
stringToUpper string = map toUpper string
