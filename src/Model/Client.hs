module Model.Client where

import Network.Socket
import Network.Socket.ByteString

import qualified Data.ByteString.Char8 as C

import Control.Concurrent
import Control.Monad

--TODO: error handling
main :: HostName -> String -> IO (Socket)
main hostname port = withSocketsDo $ do
    --create server address
    addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
    let serveraddr = head addrinfos

    --create a TCP socket for the incoming data
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    setSocketOption sock KeepAlive 1

    --connect to server
    connect sock (addrAddress serveraddr)
    
    return sock
    
listenToServer :: Socket -> IO (String,String)
listenToServer sock = do
    clientIn <- recv sock 4096

    if not $ C.null clientIn 

    then do
        let clientInString = C.unpack clientIn
            command = head $ words clientInString
            argument = concat $ tail $ words clientInString
               
        return (command,argument)
    else return ("","")
                 
waitForMessage :: Socket -> String -> IO (String,String)
waitForMessage sock msg = do
    clientIn <- recv sock 4096
    
    if not $ C.null clientIn 
       then do
           let clientInString = C.unpack clientIn
               command = head $ words clientInString
               arguments = concat $ tail $ words clientInString
               
           if command == msg then return (command,arguments)
                             else waitForMessage sock msg
       else do
           --server dc do error handling
           return ("","")
           
                 
sendMessage :: Socket -> String -> IO ()
sendMessage sock message = do
    sendAll sock (C.pack message)
