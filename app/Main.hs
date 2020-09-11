module Main where

import Shell
import ChessMain

--asks if player wants to play on shell or on gui and acts according to player input
main :: IO ()
main = do
    putStrLn "You wanna play on shell or gui? [Shell/GUI]"
    input <- getLine
    case stringToUpper input of 
         "SHELL" -> Shell.startShell
         "GUI" -> ChessMain.startGUI
         _ -> printErrorMessage "Invalid input!"
