module Shell ( startShell, printErrorMessage, stringToUpper ) where

import Data.Char    

import Model.Player
    
error_msg = "Error! "    
    
startShell :: IO ()
startShell = do 
    putStrLn "Choose gamemode! [Hotseat/Single/Network]"
    chooseGamemode
    
    
chooseGamemode :: IO ()
chooseGamemode = do
    input <- getLine
    case stringToUpper input of
         "HOTSEAT" -> putStrLn "start hotseat game"
         "SINGLE" -> do
             putStrLn "start single game"
             putStrLn "Choose your color! [White/Black]"
             player <- chooseColor
             putStrLn ("Chosen player: " ++ show player)
         "NETWORK" -> do
             putStrLn "start network game"
             putStrLn "Choose your color! [White/Black]"
             player <- chooseColor
             putStrLn ("Chosen player: " ++ show player)
         _ -> do
             printErrorMessage "Invalid input"
             chooseGamemode
              
chooseColor :: IO (Player)
chooseColor = do
    input <- getLine
    case stringToUpper input of
         "WHITE" -> return White
         "BLACK" -> return Black
         _ -> do 
             printErrorMessage "Invalid input!"
             chooseColor
             
--next action during game! TODO: game as argument
chooseAction :: IO ()
chooseAction = do
    putStrLn "TODO: if gameover -> startShell"
    putStrLn "TODO: if your turn else startShell"
    putStr "It's your turn! Type ? for help.> "                  
    input <- getLine
    case stringToUpper input of
         "?" -> do
             putStrLn "TODO: help dialogue"
             chooseAction
         "NEW" -> startShell
         "MOVE" -> do
             putStrLn "TODO: move"
             chooseAction
         _ -> do
             printErrorMessage "Invalid input!"
             chooseAction

printErrorMessage :: String -> IO ()
printErrorMessage msg = putStrLn (error_msg ++ msg)

stringToUpper :: String -> String
stringToUpper string = map toUpper string
