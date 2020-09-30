module Model.Network.Server where

import qualified Data.ByteString.Char8 as C
import Data.List
import Data.Maybe
import Data.IORef

import Control.Concurrent
import Control.Monad

import Network.Socket
import Network.Socket.ByteString

type Game = (Maybe Socket,Maybe Socket)

type Games = [Game]

startServer :: String -> IO ()
startServer port = withSocketsDo $ do
    addrinfos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing (Just port)
    let serveraddr = head addrinfos
    
    -- create a TCP socket
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol

    -- bind the socket to the address and start listening
    bind sock (addrAddress serveraddr)
    listen sock 1
    putStrLn $ "Listening on port " ++ port
    
    acceptConnections sock
    
acceptConnections :: Socket -> IO ()
acceptConnections sock = do
    connectionsRef <- newIORef [] 
    gamesRef <- newIORef []
    forever $ do
        (client,addr) <- accept sock
        putStrLn "accepted new connection"
        modifyIORef connectionsRef $ \connections -> connections ++ [client]
        
        readIORef connectionsRef >>= \conns -> putStrLn $ show conns
        
        forkIO $ runConnection client connectionsRef gamesRef
        
        return ()
    
runConnection :: Socket -> IORef [Socket] -> IORef Games -> IO ()
runConnection sock connectionsRef gamesRef = do
    --accept messages from client
    serverIn <- recv sock 4096
    
    if (not $ C.null serverIn)
       then do
           putStr "received: "
           C.putStrLn serverIn
           
           let serverInString = C.unpack serverIn
               command = head $ words serverInString
               argument = head $ tail $ words serverInString
               
           case command of 
                "requestLobby" -> do
                    putStr "send: "
                    lobby <- readIORef gamesRef
                    C.putStrLn (C.pack $ "updateLobby " ++ show (lobbyToBool lobby))
                    sendAll sock (C.pack $ "updateLobby " ++ show (lobbyToBool lobby))
                "new" ->  do 
                    if argument == "White" then modifyIORef gamesRef $ \games -> games ++ [(Just sock,Nothing)]
                                           else modifyIORef gamesRef $ \games -> games ++ [(Nothing,Just sock)]
                    putStr "send: "
                    newLobby <- readIORef gamesRef
                    C.putStrLn (C.pack $ "updateLobby " ++ show (lobbyToBool newLobby))
                    sendToAllClients connectionsRef (C.pack $ "updateLobby " ++ show (lobbyToBool newLobby))
                "join" -> do
                    oldLobby <- readIORef gamesRef
                    
                    let gameID = read argument::Int
                        (a,old:b) = splitAt (gameID-1) oldLobby
                        newElem = joinGame old sock
                        newLobby = a ++ newElem:b
                        
                    writeIORef gamesRef newLobby
                    newLobby <- readIORef gamesRef
                    
                    case getGame sock newLobby of
                         Just game -> do 
                             sendOtherPlayer sock game (C.pack "start")
                             putStrLn "start sent to game"
                         Nothing -> putStrLn "ERROR, something went wrong"
                    
                    C.putStrLn (C.pack $ "updateLobby " ++ show (lobbyToBool newLobby))
                    sendToAllClients connectionsRef (C.pack $ "updateLobby " ++ show (lobbyToBool newLobby))
                "move" -> do
                    lobby <- readIORef gamesRef
                    
                    case getGame sock lobby of
                         Just game -> do 
                             sendOtherPlayer sock game serverIn
                             putStr "sent to game "
                             C.putStrLn serverIn
                         Nothing -> putStrLn "ERROR, something went wrong"
                "quit" -> do
                    lobby <- readIORef gamesRef
                    
                    case getGame sock lobby of
                         Just game -> do
                             modifyIORef gamesRef $ \games -> delete game games
                            
                             newLobby <- readIORef gamesRef
                             
                             C.putStrLn (C.pack $ "updateLobby " ++ show (lobbyToBool newLobby))
                             sendToAllClients connectionsRef (C.pack $ "updateLobby " ++ show (lobbyToBool newLobby))
                             
                             putStr "sent to game: quit" 
                             sendOtherPlayer sock game serverIn
                         Nothing -> putStrLn "game already deleted"
                _ -> putStrLn "Unhandled messageType"

           runConnection sock connectionsRef gamesRef
       else do
           sendAll sock (C.pack "dc") --send dc back to client so client can do cleanup actions
           putStrLn $ show sock ++ "disconnected"
           modifyIORef connectionsRef $ \connections -> delete sock connections
           
    
sendToAllClients :: IORef [Socket] -> C.ByteString -> IO ()
sendToAllClients connectionsRef message = (readIORef connectionsRef) >>= \acceptedConnections -> forM_ acceptedConnections (\connection -> sendAll connection message)

sendOtherPlayer :: Socket -> Game -> C.ByteString -> IO ()
sendOtherPlayer sock (Just x, Just y) message | x == sock = sendAll y message
                                              | otherwise = sendAll x message
sendOtherPlayer sock (Just x, _) message = if x == sock then return ()
                                                        else sendAll x message
sendOtherPlayer sock (_, Just y) message = if y == sock then return ()
                                                        else sendAll y message

lobbyToBool :: Games -> [(Bool,Bool)]
lobbyToBool [] = []
lobbyToBool ((x,y):t) = [(isJust x, isJust y)] ++ lobbyToBool t

joinGame :: Game -> Socket -> Game
joinGame (Just x, Nothing) sock = (Just x, Just sock)
joinGame (Nothing, Just x) sock = (Just sock, Just x)

isInGame :: Socket -> Game -> Bool
isInGame sock (x,y) = x == Just sock || y == Just sock
                                                          
getGame :: Socket -> Games -> Maybe Game
getGame sock [] = Nothing
getGame sock (h:t) = if isInGame sock h then Just h
                                        else getGame sock t
