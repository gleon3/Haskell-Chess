module View.LobbyView where

import Graphics.UI.Gtk

import Model.Client
import Model.Model
import Model.Player
import Model.Phase
import View.ChessView

import Control.Concurrent
import Control.Monad
import Data.IORef
import Network.Socket
import Network.Socket.ByteString
import qualified Data.ByteString.Char8 as C

import Control.Exception

import System.Exit

showLobby :: Socket -> IO ()
showLobby sock = do
    --TODO: exit handling close window etc.
    initGUI
    
    --keep track of when to exit loop in lobbyThread
    exit <- newEmptyMVar
   
    window <- windowNew
    set window [windowTitle := "Lobby", containerBorderWidth := 0, windowDefaultHeight := 700, windowDefaultWidth := 700]
    
    vbox <- vBoxNew False 10
    containerAdd window vbox
    
    newButton <- buttonNew
    set newButton [ buttonLabel := "New Game" ]
    boxPackEnd vbox newButton PackNatural 0
    
    on newButton buttonActivated $ do
        
        dialog <- messageDialogNew (Just window) [DialogDestroyWithParent, DialogModal] MessageQuestion ButtonsNone "Which color do you want to play as?"
        dialogAddButton dialog "White" (ResponseUser 0)
        dialogAddButton dialog "Black" (ResponseUser 1)
        set dialog [windowTitle := "Choose color"]
        result <- dialogRun dialog
        widgetDestroy dialog
        
        case result of
             ResponseUser 0 -> do
                 putStrLn $ "new game as white" 
                 sendMessage sock "new White"
                 putMVar exit True
                 widgetHide window
                 mainQuit
                 finally (setupBoard (newNetworkChess Waiting White sock)) (showLobby sock)
             ResponseUser 1 -> do
                 putStrLn $ "new game as black" 
                 sendMessage sock "new Black"
                 putMVar exit True
                 widgetHide window
                 mainQuit
                 finally (setupBoard (newNetworkChess Waiting Black sock)) (showLobby sock)
             ResponseUser _ -> error "unhandled response"
             _ -> return ()
             
    scrollWin <- scrolledWindowNew Nothing Nothing
    containerAdd vbox scrollWin
    
    lobbyBox <- vBoxNew False 10
    --containerAdd vbox lobbyBox
    scrolledWindowAddWithViewport scrollWin lobbyBox
    
    --requests the current lobby from server, so the lobby can be updated the first time client joins lobby
    sendMessage sock "requestLobby"
    
    --keep listening for server lobby updating
    lobbyThread <- forkIO $ do
        let listenToUpdateLobby = do
                (com, arg) <- waitForMessage sock "updateLobby"
            
                --also keep track of index
                let newLobby = read arg::[(Bool,Bool)]
                    newLobby' = zip [1..] newLobby
                
                --remove old lobby
                oldLobby <- containerGetChildren lobbyBox
                forM_ oldLobby (\game -> do
                    containerRemove lobbyBox game)
            
                --add new lobby, doesn't show running games
                forM_ newLobby' (\(i, (white,black)) -> unless (white && black) $ do
                    gameBox <- hBoxNew True 10
                    
                    whiteButton <- buttonNew
                    set whiteButton [ buttonLabel := "White" ]
                    containerAdd gameBox whiteButton
                    
                    --if white is true, game already has white player in, so disable the button
                    if white then widgetSetSensitive whiteButton False
                            else widgetSetSensitive whiteButton True
                            
                    on whiteButton buttonActivated $ do
                        putStrLn $ "join game " ++ show i ++ " as white"
                        sendMessage sock ("join " ++ show i)
                        putMVar exit True
                        widgetHide window
                        mainQuit
                        finally (setupBoard (newNetworkChess Running White sock)) (showLobby sock)
                        
                    
                    blackButton <- buttonNew
                    set blackButton [ buttonLabel := "Black" ]
                    containerAdd gameBox blackButton
                    
                    --if black is true, game already has black player in, so disable the button
                    if black then widgetSetSensitive blackButton False
                            else widgetSetSensitive blackButton True
                    
                    on blackButton buttonActivated $ do
                        putStrLn $ "join game " ++ show i ++ " as black"
                        sendMessage sock ("join " ++ show i)
                        putMVar exit True
                        widgetHide window
                        mainQuit
                        finally (setupBoard (newNetworkChess Running Black sock)) (showLobby sock)
                    
                    postGUIAsync $ boxPackStart lobbyBox gameBox PackNatural 0)
                    
                postGUIAsync $ widgetShowAll lobbyBox
                
                exit' <- tryTakeMVar exit
                
                if exit' == Just True then return ()
                                      else listenToUpdateLobby
        listenToUpdateLobby
        
    on window objectDestroy $ do
        putMVar exit True
        shutdown sock ShutdownBoth
        mainQuit
        
    widgetShowAll window
    mainGUI
