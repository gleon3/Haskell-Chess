module View.StartView where

import Graphics.UI.Gtk

import View.ChessView

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
    
    on buttonHotseat buttonActivated $ do
        showModelNew 
    on buttonSingle buttonActivated $ do
        playerDialog <- windowNew
        set playerDialog [windowTitle := "Choose Color", windowDefaultHeight := 100, windowDefaultWidth := 200, windowModal := True]
        windowSetTypeHint playerDialog WindowTypeHintDialog
        
        vbox <- vBoxNew False 5
        
        containerAdd playerDialog vbox
        
        question <- labelNew (Just "Choose your color!")
        buttonWhite <- buttonNew
        buttonBlack <- buttonNew
        
        set buttonWhite [ buttonLabel := "White" ]
        set buttonBlack [ buttonLabel := "Black" ]
        
        containerAdd vbox question
        containerAdd vbox buttonWhite
        containerAdd vbox buttonBlack
        
        on buttonWhite buttonActivated (putStrLn "White")
        on buttonBlack buttonActivated (putStrLn "Black")
        
        widgetShowAll playerDialog
    on buttonNetwork buttonActivated $ do
        txt <- entryGetText serverAdress
        putStrLn txt
        putStrLn ("TODO: check if can connect then go into lobby, else give error message!")
        
    on window objectDestroy mainQuit
    widgetShowAll window
    mainGUI
