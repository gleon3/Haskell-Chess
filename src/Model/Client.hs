module Model.Client where

import Network.Socket
import Network.Socket.ByteString

import qualified Data.ByteString.Char8 as C

import Control.Concurrent
import Control.Monad

import qualified Control.Exception as E

--TODO: error handling
main :: HostName -> String -> IO ()
main hostname port = withSocketsDo $ do
    --create server address
    addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
    let serveraddr = head addrinfos

    --create a TCP socket for the incoming data
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    setSocketOption sock KeepAlive 1

    --connec tto server
    connect sock (addrAddress serveraddr)
    
    E.finally (listenToServer sock) (putStr "hi")
    return ()
    
listenToServer :: Socket -> IO ()
listenToServer sock = do
    forever $ do
        clientIn <- recv sock 1024
        putStr "received: "
        C.putStrLn clientIn
