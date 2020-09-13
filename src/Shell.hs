module Shell ( startShell ) where

import Data.Char    

import Model.Model
import Model.Player
import Model.GameState
import Model.Phase
import Model.Move
import Model.Cell
import Model.Piece
import Model.GameField
import Model.AiChess

import Model.Client

import Data.IORef
import Control.Monad
import Control.Concurrent
import Network.Socket

import Control.Exception

import System.IO

error_msg = "Error! "    
port = "5000"
    
startShell :: IO ()
startShell = do 
    hSetBuffering stdout NoBuffering
    putStrLn "Choose gamemode! [Hotseat/Single/Network]. To quit type nothing!"
    chooseGamemode
    
chooseGamemode :: IO ()
chooseGamemode = do
    input <- getLine
    case stringToUpper input of
         "HOTSEAT" -> do
             stateUpdate newModel
         "SINGLE" -> do
             putStrLn "Choose your color! [White/Black]"
             player <- chooseColor
             stateUpdate $ newAiModel player
         "NETWORK" -> do
             putStrLn "Input ip to join!"
             input <- getLine
             
             sock <- main input "5000"
             
             serverLobby sock
         "" -> putStrLn "Bye!"
         _ -> do
             printErrorMessage "Invalid input"
             chooseGamemode
    where newModel = newChess Running
          newAiModel = newAiChess Running
              
chooseColor :: IO (Player)
chooseColor = do
    input <- getLine
    case stringToUpper input of
         "WHITE" -> return White
         "BLACK" -> return Black
         _ -> do 
             printErrorMessage "Invalid input!"
             chooseColor
             
--is only called once the model updated!
stateUpdate :: Model -> IO ()
stateUpdate model = do
    if getCurrentPhase state == Waiting
       then do
           case model of
                NetworkChess state player sock -> do
                    putStrLn "Waiting for other player to join..."
                    clientIn <- waitForMessage sock "start"
                    
                    putStrLn "Other player joined, game is now starting!"
                    stateUpdate (NetworkChess (setCurrentPhase state Running) player sock)
       else return ()
       
    if getCurrentPhase state == Finished 
       then do
           putStr "Game over! "
           case winner state of
                Just x -> putStrLn $ show x ++ " has won!"
                _ -> putStrLn "It's a draw!"
           
           startShell
          else do
              putStrLn $ show state
              chooseAction model
    where state = getState model

--next action during game!
chooseAction :: Model -> IO ()
chooseAction model = if not $ isYourTurn model
                        then do
                            case model of
                                 AiChess _ player -> do
                                     putStrLn "Ai thinking..."
                                     let maximizingPlayer = if player == White then False else True --player is white -> ai is black and ai is minimizing
                                     case getAiMove 3 maximizingPlayer (getState model) of
                                          Just move -> do
                                              putStrLn $ "Ai move: " ++ show move
                                              stateUpdate $ executeMove move model
                                          Nothing -> do
                                              printErrorMessage "Ai coudln't move!"
                                              chooseAction model
                                 NetworkChess _ _ sock -> do
                                     putStrLn "Wait for opponent to move..."
                                     (com,arg) <- waitForMessage sock "move"
                                
                                     let move = read arg::Move
                                     
                                     putStrLn $ "Opponent made move: " ++ show move
                                     stateUpdate $ executeMove move model
                        else do
                            putStr "It's your turn! Type ? for help> "
                            input <- getLine
                            case stringToUpper input of
                                "?" ->  do
                                    showHelp
                                    chooseAction model
                                "NEW" -> case model of
                                              NetworkChess _ _ sock -> close sock >> startShell
                                              _ -> startShell
                                "MOVES" -> do
                                    putStrLn $ show $ getLegalMoves model
                                    chooseAction model
                                "CASTLE" -> do
                                    putStrLn "Which side do you want to castle to? [King/Queen]"
                                    kingside <- chooseCastle
                                    case move (Castle kingside) model of
                                            Left newModel -> case model of
                                                                  NetworkChess _ _ sock -> do
                                                                      sendMessage sock ("move " ++ show (Castle kingside))
                                                                      stateUpdate newModel
                                                                  _ -> stateUpdate newModel
                                            Right _ -> do 
                                                if kingside then putStrLn "Couldn't castle kingside!"
                                                            else putStrLn "Couldn't castle queenside!"
                                                chooseAction model
                                "QUIT" -> do
                                    putStrLn "Thanks for playing!"
                                    case model of
                                              NetworkChess _ _ sock -> do
                                                  sendMessage sock "quit"
                                                  serverLobby sock
                                              _ -> startShell
                                _ -> let moveInputCells = splitAt 2 input
                                     in if (checkValidCellFormat $ fst moveInputCells) && (checkValidCellFormat $ snd moveInputCells) 
                                           then let from = parseCell $ fst moveInputCells
                                                    to = parseCell $ snd moveInputCells
                                                    moveType = getTypeOfMove from to (getState model)
                                                in case moveType of
                                                        PawnPromotion from to _ -> case move (PawnPromotion from to Queen) model of
                                                                                        Left newModel -> do
                                                                                            putStrLn "Choose piece to promote to! [Queen/Rook/Bishop/Knight]"
                                                                                            piece <- promoteDialog
                                                                                            putStrLn $ "Your move: " ++ show (PawnPromotion from to piece)
                                                                                            case model of
                                                                                                 NetworkChess _ _ sock -> do
                                                                                                     sendMessage sock ("move " ++ show (PawnPromotion from to piece))
                                                                                                     stateUpdate $ executeMove (PawnPromotion from to piece) model 
                                                                                                 _ -> stateUpdate $ executeMove (PawnPromotion from to piece) model 
                                                                                        _ -> do
                                                                                            printErrorMessage "Invalid move!"
                                                                                            chooseAction model
                                                        mv -> case move mv model of
                                                                   Left newModel -> do
                                                                       putStrLn $ "Your move: " ++ show mv
                                                                       case model of
                                                                            NetworkChess _ _ sock -> do 
                                                                                sendMessage sock ("move " ++ show mv)
                                                                                stateUpdate newModel
                                                                            _ -> stateUpdate newModel
                                                                   _ -> do 
                                                                       printErrorMessage $ "Invalid move!"
                                                                       chooseAction model
                                           else 
                                           if (checkValidCellFormat $ fst moveInputCells) && (snd moveInputCells == []) 
                                              then let cell = parseCell $ fst moveInputCells
                                                   in if isWithinBounds cell 
                                                          then do
                                                              putStrLn $ show $ getPossibleMovesForPiece cell model
                                                              chooseAction model
                                                          else do
                                                              printErrorMessage "Invalid input!"
                                                              chooseAction model
                                              else do
                                                  printErrorMessage "Invalid input!"
                                                  chooseAction model
                                                
showHelp :: IO ()
showHelp = do
    putStrLn "Type:"
    putStrLn "?: for help"
    putStrLn "NEW: to start a new game"
    putStrLn "a1: show possible moves for piece at cell a1"
    putStrLn "MOVES: to show possible moves"
    putStrLn "CASTLE: to perform a castling move"
    putStrLn "a1a2: move from a1 to a2"
    putStrLn "QUIT: to exit current game"
          
chooseCastle :: IO (Bool)
chooseCastle = do
    input <- getLine
    case stringToUpper input of
         "KING" -> return True
         "QUEEN" -> return False
         _ -> do 
             printErrorMessage "Invalid input!"
             chooseCastle
             
promoteDialog :: IO (PieceType)
promoteDialog = do
    input <- getLine
    case stringToUpper input of
         "QUEEN" -> return Queen
         "ROOK" -> return Rook
         "BISHOP" -> return Bishop
         "KNIGHT" -> return Knight
         _ -> do
             printErrorMessage "Invalid input!"
             promoteDialog
             
serverLobby :: Socket -> IO ()
serverLobby sock = do
    lobbyRef <- newIORef ([]::[(Bool,Bool)])
    
    --keep listening for server lobby updating
    lobbyThread <- forkIO $ forever $ do
        --TODO:listen to quit
        (com, arg)<- waitForMessage sock "updateLobby"
        
        let newLobby = read arg::[(Bool,Bool)]
        
        writeIORef lobbyRef newLobby
        
    --requests the current lobby from server, so the lobby can be updated the first time client joins lobby
    sendMessage sock "requestLobby"
    
    chooseAction lobbyRef lobbyThread
    where
        chooseAction lobbyRef lobbyThread = do
            putStr "Lobby. Type ? for help> "
            input <- getLine
            case stringToUpper input of
                 "?" -> do
                     showHelpLobby
                     chooseAction lobbyRef lobbyThread
                 "LOBBY" -> do
                     printLobby lobbyRef
                     chooseAction lobbyRef lobbyThread
                 "NEW" -> do
                     putStrLn "Choose your color! [White/Black]"
                     player <- chooseColor
                     sendMessage sock ("new " ++ show player)
                     
                     --joining game so stop listening to lobby updates! thread doesn't have to run in background
                     killThread lobbyThread
                     
                     stateUpdate $ newNetworkChess Waiting player sock
                 "JOIN" -> do
                     putStrLn "Which game would you like to join [GameID]"
                     gameID <- joinDialog
                     
                     let gameIndex = read gameID::Int
                     lobby <- readIORef lobbyRef
                     
                     if gameIndex <= length lobby 
                        then let gameToJoin = lobby !! (gameIndex-1)
                                 white = fst gameToJoin
                                 black = snd gameToJoin
                             in if white && black 
                                   then printErrorMessage ("Couldn't join game " ++ gameID) >> chooseAction lobbyRef lobbyThread
                                   else let playerToJoin = if not white && black
                                            then White
                                            else Black
                                        in do
                                            sendMessage sock ("join " ++ gameID)
                                            --joining game so stop listening to lobby updates! thread doesn't have to run in background
                                            killThread lobbyThread
                                            stateUpdate $ newNetworkChess Running playerToJoin sock
                        else printErrorMessage ("Couldn't join game " ++ gameID) >> chooseAction lobbyRef lobbyThread
                 "QUIT" -> do
                     close sock
                     startShell
                 _ -> do
                     printErrorMessage "Invalid input!"
                     chooseAction lobbyRef lobbyThread
        
--print all joinable games, doesn't show running games
printLobby :: IORef [(Bool,Bool)] -> IO ()
printLobby lobbyRef = readIORef lobbyRef >>= \lobby ->
    if lobby == [] 
       then putStrLn "There are no games you could join! Try creating a new game"
       else do
           putStrLn "**_** indicates that spot is joinable"
           let lobby' = zip [1..] lobby
           forM_ lobby' (\(i,(white,black)) -> unless (white && black) $ do
               if white then putStrLn $ show i ++ ": White - **Black**"
                        else putStrLn $ show i ++ ": **White** - Black")
    
showHelpLobby :: IO ()
showHelpLobby =  do
    putStrLn "Type:"
    putStrLn "?: for help"
    putStrLn "NEW: to start a new network game"
    putStrLn "LOBBY: to see current joinable games"
    putStrLn "JOIN: to join a joinable game"
    putStrLn "QUIT: to exit the lobby"
        
joinDialog :: IO (String)
joinDialog = do
    input <- getLine
    if not $ elem False $ map isDigit input then return input
                                            else putStrLn "Only numbers allowed!" >> joinDialog

printErrorMessage :: String -> IO ()
printErrorMessage msg = putStrLn (error_msg ++ msg)

stringToUpper :: String -> String
stringToUpper string = map toUpper string
