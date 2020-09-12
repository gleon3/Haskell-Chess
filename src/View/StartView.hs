module View.StartView where

import Graphics.UI.Gtk

import View.ChessView
import View.LobbyView
import Model.Model
import Model.Phase
import Model.Player
import Model.Client

import Control.Exception

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
    boxPackEnd vbox serverAdress PackNatural 0

    set buttonHotseat [ buttonLabel := "Hotseat" ]
    set buttonSingle [ buttonLabel := "Single" ]
    set buttonNetwork [ buttonLabel := "Network" ]
    entrySetText serverAdress "127.0.0.1"
    
    on buttonHotseat buttonActivated $ do
        widgetDestroy window
        finally (setupBoard (newChess Running)) showMenu
    on buttonSingle buttonActivated $ do
        dialog <- messageDialogNew (Just window) [DialogDestroyWithParent, DialogModal] MessageQuestion ButtonsNone "Which color do you want to play as?"
        dialogAddButton dialog "White" (ResponseUser 0)
        dialogAddButton dialog "Black" (ResponseUser 1)
        set dialog [windowTitle := "Choose color"]
        result <- dialogRun dialog
        widgetDestroy dialog
        
        case result of
             ResponseUser 0 -> do
                 widgetDestroy window
                 finally (setupBoard (newAiChess Running White)) showMenu
             ResponseUser 1 -> do
                 widgetDestroy window
                 finally (setupBoard (newAiChess Running Black)) showMenu
             ResponseUser _ -> error "unhandled response"
             _ -> return ()
             
    on buttonNetwork buttonActivated $ do
        txt <- entryGetText serverAdress
        sock <- main txt "5000"
        widgetDestroy window
        finally (showLobby sock) showMenu
        `catch` (\e -> let err = show (e::IOException)
                       in do
                           dialog <- messageDialogNew (Just window) [DialogDestroyWithParent, DialogModal] MessageError ButtonsClose "Couldn't connect to given adress!"
                           set dialog [windowTitle := "Connection failed"]
                           
                           dialogRun dialog
                           
                           widgetDestroy dialog)
        
    on window objectDestroy mainQuit
    widgetShowAll window
    mainGUI
