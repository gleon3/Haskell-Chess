module Shell ( startShell, printErrorMessage, stringToUpper ) where

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
    
error_msg = "Error! "    
    
startShell :: IO ()
startShell = do 
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
             putStrLn "start network game"
             putStrLn "Choose your color! [White/Black]"
             player <- chooseColor
             stateUpdate $ newNetworkModel player
         "" -> putStrLn "Bye!"
         _ -> do
             printErrorMessage "Invalid input"
             chooseGamemode
    where newModel = newChess Running
          newAiModel = newAiChess Running
          newNetworkModel = newNetworkChess Running
              
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
    putStrLn $ show state
    if getCurrentPhase state /= Running 
       then startShell
       else chooseAction model
    where state = getState model

--next action during game!
chooseAction :: Model -> IO ()
chooseAction model = if not $ isYourTurn model
                        then do
                            case model of
                                 AiChess _ _ -> do
                                     putStrLn "Ai thinking..."
                                     case getAiMove 2 True (getState model) of
                                          Just move -> do
                                              putStrLn $ "Ai move: " ++ show move
                                              stateUpdate $ executeMove move model
                                          Nothing -> do
                                              printErrorMessage "Ai coudln't move!"
                                              chooseAction model
                                 NetworkChess _ _ -> putStrLn "NETWORK TODO"
                        else do
                            putStr "It's your turn! Type ? for help> "                  
                            input <- getLine
                            case stringToUpper input of
                                "?" ->  do
                                    showHelp
                                    chooseAction model
                                "NEW" -> startShell
                                "MOVES" -> do
                                    putStrLn $ show $ getLegalMoves model
                                    chooseAction model
                                "CASTLE" -> do
                                    putStrLn "Which side do you want to castle to? [King/Queen]"
                                    kingside <- chooseCastle
                                    case move (Castle kingside) model of
                                            Left newModel -> stateUpdate newModel
                                            Right _ -> do 
                                                if kingside then putStrLn "Couldn't castle kingside!"
                                                            else putStrLn "Couldn't castle queenside!"
                                                chooseAction model
                                "QUIT" -> do
                                    putStrLn "Thanks for playing!"
                                    startShell
                                _ -> let moveInputCells = splitAt 2 input
                                     in if (checkValidCellFormat $ fst moveInputCells) && (checkValidCellFormat $ snd moveInputCells) 
                                           then let from = parseCell $ fst moveInputCells
                                                    to = parseCell $ snd moveInputCells
                                                    moveType = getTypeOfMove from to (getState model)
                                                in case moveType  of
                                                        PawnPromotion from to _ -> case move (PawnPromotion from to Queen) model of
                                                                                        Left newModel -> do
                                                                                            putStrLn "Choose piece to promote to! [Queen/Rook/Bishop/Knight]"
                                                                                            piece <- promoteDialogue
                                                                                            putStrLn $ "Your move: " ++ show (PawnPromotion from to piece)
                                                                                            stateUpdate $ executeMove (PawnPromotion from to piece) model 
                                                                                        _ -> do
                                                                                            printErrorMessage "Invalid move!"
                                                                                            chooseAction model
                                                        mv -> case move mv model of
                                                                   Left newModel -> do
                                                                       putStrLn $ "Your move: " ++ show mv
                                                                       stateUpdate newModel
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
             
promoteDialogue :: IO (PieceType)
promoteDialogue = do
    input <- getLine
    case stringToUpper input of
         "QUEEN" -> return Queen
         "ROOK" -> return Rook
         "BISHOP" -> return Bishop
         "KNIGHT" -> return Knight
         _ -> do
             printErrorMessage "Invalid input!"
             promoteDialogue

printErrorMessage :: String -> IO ()
printErrorMessage msg = putStrLn (error_msg ++ msg)

stringToUpper :: String -> String
stringToUpper string = map toUpper string
