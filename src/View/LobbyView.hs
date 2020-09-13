module View.LobbyView ( showLobby ) where

import Control.Concurrent
import Control.Exception
import Control.Monad

import qualified Data.ByteString.Char8 as C
import Data.IORef

import Graphics.UI.Gtk

import Model.Network.Client
import Model.Model
import Model.Phase
import Model.Player

import Network.Socket
import Network.Socket.ByteString

import View.ChessView

showLobby :: Socket -> IO ()
showLobby sock = do
    --TODO: exit handling close window etc.
    initGUI
    
    --keep track of when to exit loop in lobbyThread
    exit <- newIORef False
   
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
                 writeIORef exit True
                 widgetDestroy window
                 finally (setupBoard (newNetworkChess Waiting White sock)) (showLobby sock)
             ResponseUser 1 -> do
                 putStrLn $ "new game as black" 
                 sendMessage sock "new Black"
                 writeIORef exit True
                 widgetDestroy window
                 finally (setupBoard (newNetworkChess Waiting Black sock)) (showLobby sock)
             ResponseUser _ -> error "unhandled response"
             _ -> return ()
             
    scrollWin <- scrolledWindowNew Nothing Nothing
    containerAdd vbox scrollWin
    
    lobbyBox <- vBoxNew False 10
    --containerAdd vbox lobbyBox
    scrolledWindowAddWithViewport scrollWin lobbyBox
    
    --keep listening for server lobby updating
    lobbyThread <- forkIO $
        let handleUpdateLobby = do
                (com, arg) <- listenToServer sock
                
                putStrLn $ com ++ " " ++ arg
                
                case com of
                     "updateLobby" -> do
                         --also keep track of index
                         let newLobby = read arg::[(Bool,Bool)]
                             newLobby' = zip [1..] newLobby
                           
                         --remove old lobby
                         oldLobby <- containerGetChildren lobbyBox
                         forM_ oldLobby (\game -> containerRemove lobbyBox game)
                         
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
                                 writeIORef exit True
                                 widgetDestroy window
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
                                 writeIORef exit True
                                 widgetDestroy window
                                 finally (setupBoard (newNetworkChess Running Black sock)) (showLobby sock)
                             
                             postGUIAsync $ boxPackStart lobbyBox gameBox PackNatural 0)
                             
                         postGUIAsync $ widgetShowAll lobbyBox
                         
                         exit' <- readIORef exit
                         yield
                         
                         if exit' then putStrLn "updateLobbyThread ended"
                                  else handleUpdateLobby
                     "dc" -> putStrLn "updateLobbyThread ended" --disconnect from server
                     _ -> error "ERROR, GOT START, MOVE, QUIT MESSAGE WHILE NOT BEING IN GAME"
        in handleUpdateLobby
        
    --requests the current lobby from server, so the lobby can be updated the first time client joins lobby
    sendMessage sock "requestLobby"
        
    on window objectDestroy $ do
        exit' <- readIORef exit
        --exit' is true when somebody creates/joins game, if somebody closes window shutdown sock, so server can handle with disconnect
        unless exit' $ do
            writeIORef exit True
            shutdown sock ShutdownBoth
            
        mainQuit
        
    widgetShowAll window
    mainGUI
