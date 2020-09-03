module Model.Server where

import Network.Socket
import Network.Socket.ByteString
import Control.Concurrent
import Control.Monad
import qualified Data.ByteString.Char8 as C
import qualified Control.Exception as E
import Data.IORef

data Game = Int

type Games = [Game]

maxConnections = 1
port = "5000"

main :: IO ()
main = withSocketsDo $ do
    addrinfos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing (Just port)
    let serveraddr = head addrinfos
    
    -- create a TCP socket
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol

    -- bind the socket to the address and start listening
    bind sock (addrAddress serveraddr)
    listen sock maxConnections
    
    acceptConnections sock
    
acceptConnections :: Socket -> IO ()
acceptConnections sock = do
    connectionsRef <- newIORef [] 
    forever $ do
        (client,addr) <- accept sock
        modifyIORef connectionsRef $ \connections -> connections ++ [client]
        forkIO $ runConnection client connectionsRef
        
        return ()
    
runConnection :: Socket -> IORef [Socket] -> IO ()
runConnection sock connectionsRef = do
    serverIn <- recv sock 1024
    unless (C.null serverIn) $ do
           putStr "received: "
           C.putStrLn serverIn
           putStr "send: "
           C.putStrLn serverIn
           sendToAllClients connectionsRef serverIn
           runConnection sock connectionsRef
    
sendToAllClients :: IORef [Socket] -> C.ByteString -> IO ()
sendToAllClients connectionsRef message = (readIORef connectionsRef) >>= \acceptedConnections -> forM_ acceptedConnections (\connection -> sendAll connection message)
