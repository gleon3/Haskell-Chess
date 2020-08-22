module Main where

import Shell
import ChessMain

main :: IO ()
main = do
    putStrLn "You wanna play on shell or gui? [Shell/GUI]"
    input <- getLine
    case input of 
         "Shell" -> Shell.someFunc
         "GUI" -> ChessMain.someFunc
         _ -> putStrLn "TODO: printError" >> main
       
