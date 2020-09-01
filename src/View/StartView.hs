module View.StartView where

import Graphics.UI.Gtk

import View.ChessView
import Model.Model
import Model.Phase
import Model.Player

showMenu :: IO ()
showMenu = do
    initGUI
    window <- windowNew
    
    set window [windowTitle := "Chess", containerBorderWidth := 0, windowDefaultHeight := 250, windowDefaultWidth := 250]
    
    vbox <- vBoxNew False 5
    
    containerAdd window vbox
    
    buttonHotseat <- buttonNew
    buttonSingle <- buttonNew
    buttonNetwork <- buttonNew
    serverAdress <- entryNew
    
    containerAdd vbox buttonHotseat
    containerAdd vbox buttonSingle
    containerAdd vbox buttonNetwork
    containerAdd vbox serverAdress

    set buttonHotseat [ buttonLabel := "Hotseat" ]
    set buttonSingle [ buttonLabel := "Single" ]
    set buttonNetwork [ buttonLabel := "Network" ]
    entrySetText serverAdress "127.0.0.1"
    
    on buttonHotseat buttonActivated (setupBoard (newChess Running))
    on buttonSingle buttonActivated $ do
        dialog <- messageDialogNew (Just window) [DialogDestroyWithParent, DialogModal] MessageQuestion ButtonsNone "Which color do you want to play as?"
        dialogAddButton dialog "White" (ResponseUser 0)
        dialogAddButton dialog "Black" (ResponseUser 1)
        set dialog [windowTitle := "Choose color"]
        result <- dialogRun dialog
        widgetDestroy dialog
        
        case result of
             ResponseUser 0 -> setupBoard (newAiChess Running White)
             ResponseUser 1 -> setupBoard (newAiChess Running Black)
             ResponseUser _ -> error "unhandled response"
             _ -> return ()
             
    on buttonNetwork buttonActivated $ do
        txt <- entryGetText serverAdress
        putStrLn txt
        putStrLn ("TODO: check if can connect then go into lobby, else give error message!")
        
    on window objectDestroy mainQuit
    widgetShowAll window
    mainGUI
