module View.ChessView where

import Graphics.Rendering.Cairo
import Graphics.Rendering.Cairo.SVG
import Graphics.UI.Gtk hiding (rectangle,get)

import qualified Data.ByteString.Char8 as C
import Network.Socket.ByteString

import Model.Model
import Model.AiChess
import Model.GameState
import Model.GameField hiding (set)
import Model.Phase
import Model.Move
import Model.Cell
import Model.Piece
import Model.Player
import Model.Client
import Control.Monad
import Control.Concurrent

import Data.Array
import Data.List
import Data.IORef

data GUIState = GUIState { selectedCell :: Maybe Cell, currentModel :: Model }

initGUIState :: Model -> IO (IORef GUIState)
initGUIState model = newIORef $ GUIState { selectedCell = Nothing, currentModel = model }

setupBoard :: Model -> IO ()
setupBoard model = do
    --TODO: exit handling close window etc.
    initGUI
    window <- windowNew
    
    stateRef <- initGUIState model
    
    canvas <- drawingAreaNew
    
    set window [containerBorderWidth := 0, windowTitle := "Chess", containerChild := canvas, windowDefaultHeight := 700, windowDefaultWidth := 700]
    
    on canvas draw $ do
        width <- liftIO $ widgetGetAllocatedWidth canvas
        height <- liftIO $ widgetGetAllocatedHeight canvas
        
        liftIO (readIORef stateRef) >>= \GUIState { selectedCell = currentSelected, currentModel = curModel } ->
            if (currentPhase $ getState curModel) == Waiting 
               then do
                   setSourceRGB 0.0 0.0 0.0
                   extents <- textExtents "Waiting for other player to join..."
                   moveTo ((fromIntegral width)/2 - (textExtentsWidth extents)/2) ((fromIntegral height)/2) 
                   showText "Waiting for other player to join..."
               else do
                   let cellSize' = min ((fromIntegral width)/(fromIntegral size)) ((fromIntegral height)/(fromIntegral size))
                       cellSize = round cellSize'
                    
                   drawBoard cellSize
                           
               
                   --draw selected cell and possible moves from given cell
                   case currentSelected of 
                       Just cell -> do
                           setSourceRGB 0.4 0.4 0.25
                           drawSquare cell cellSize
                           setSourceRGB 0.51 0.6 0.41
                           drawMoves (getPossibleMovesForPiece cell curModel) cellSize
                       Nothing -> return ()
                               
                   --make check more visible to player!
                   if isChecked (currentPlayer $ getState curModel) (getState curModel) 
                     then do
                         setSourceRGB 1.0 0.0 0.0
                         case getCellOfPiece (Piece (currentPlayer $ getState curModel) King) (gameField $ getState curModel) of
                               Just cell -> drawSquare cell cellSize
                               Nothing -> return ()
                     else return ()     
                     
                   --draw all pieces on the board
                   drawPieces (gameField (getState curModel)) cellSize
                   
                   --read new state and check if game ended after making move and give dialog
                   {-liftIO (readIORef stateRef) >>= \GUIState { selectedCell = newCurrentSelected, currentModel = newCurModel } -> 
                        if (currentPhase $ getState newCurModel) == Finished 
                           then do 
                               dialog <- liftIO $ messageDialogNew (Just window) [DialogDestroyWithParent, DialogModal] MessageInfo ButtonsClose "Game over!"
                               
                               case winner $ getState newCurModel of
                                    Just player -> liftIO $ messageDialogSetMarkup dialog ("Game over! " ++ show player ++ " has won!")
                                    Nothing -> liftIO $ messageDialogSetMarkup dialog "Game over! It's a draw!"
                                
                               liftIO $ set dialog [windowTitle := "Game over"]
                               result <- liftIO $ dialogRun dialog
                               
                               --action to be taken after clicked dialog, in this case destroy the window, which also destroys the dialog!
                               liftIO $ widgetDestroy window
                               else return () -}
            
                                                                
    on canvas buttonPressEvent $ do
        liftIO (readIORef stateRef) >>= \GUIState { selectedCell = currentSelected, currentModel = curModel } ->
            if (currentPhase $ getState curModel) == Running 
               then do
                   width <- liftIO $ widgetGetAllocatedWidth canvas
                   height <- liftIO $ widgetGetAllocatedHeight canvas
                   
                   button <- eventButton
                   (x,y) <- eventCoordinates
                   
                   let cellSize' = min ((fromIntegral width)/(fromIntegral size)) ((fromIntegral height)/(fromIntegral size))
                       cellSize = round cellSize'
                       cell = (7-(floor $ y/(fromIntegral cellSize)),(floor $ x/(fromIntegral cellSize)))
                   
                   liftIO $ case currentSelected of
                        Nothing ->  if get cell (gameField $ getState curModel) == Nothing
                                    then modifyIORef stateRef $ \state -> state { selectedCell = Nothing }
                                    else modifyIORef stateRef $ \state -> state { selectedCell = Just cell }
                        Just selected -> case find (\move -> getTargetCell move == cell) (getPossibleMovesForPiece selected curModel) of
                                            Just (PawnPromotion from to _) -> do
                                                dialog <- messageDialogNew (Just window) [DialogDestroyWithParent, DialogModal] MessageQuestion ButtonsNone "Which piece do you want to promote to?"
                                                dialogAddButton dialog "Queen" (ResponseUser 0)
                                                dialogAddButton dialog "Rook" (ResponseUser 1)
                                                dialogAddButton dialog "Bishop" (ResponseUser 2)
                                                dialogAddButton dialog "Knight" (ResponseUser 3)
                                                set dialog [windowTitle := "PawnPromotion"]
                                                result <- dialogRun dialog
                                                widgetDestroy dialog
                                                case result of
                                                   ResponseUser 0 -> makeMove stateRef (PawnPromotion from to Queen)
                                                   ResponseUser 1 -> makeMove stateRef (PawnPromotion from to Rook)
                                                   ResponseUser 2 -> makeMove stateRef (PawnPromotion from to Bishop)
                                                   ResponseUser 3 -> makeMove stateRef (PawnPromotion from to Knight)
                                                   ResponseUser _ -> error "unhandled response"
                                                   _ -> makeMove stateRef (PawnPromotion from to Queen)
                                            Just move -> makeMove stateRef move
                                            _ -> if get cell (gameField $ getState curModel) == Nothing
                                                    then modifyIORef stateRef $ \state -> state { selectedCell = Nothing }
                                                    else modifyIORef stateRef $ \state -> state { selectedCell = Just cell }
                   
                   liftIO $ widgetQueueDraw canvas
                   return True
               else return False
  
    --keeps track of if game finished
    forkIO $ forever $ do
        state <- readIORef stateRef
        if (currentPhase $ getState $ currentModel state) /= Finished
           then putStr "" --do nothing, return (), when, unless freeze program
           else postGUISync $ do
    
               dialog <- messageDialogNew (Just window) [DialogDestroyWithParent, DialogModal] MessageInfo ButtonsClose "Game over!"
               
               case winner $ getState $ currentModel state of
                    Just player -> messageDialogSetMarkup dialog ("Game over! " ++ show player ++ " has won!")
                    Nothing -> messageDialogSetMarkup dialog "Game over! It's a draw!"
                    
               set dialog [windowTitle := "Game over"]
               
               result <- dialogRun dialog
               
               --action to be taken after clicked dialog, in this case destroy the window, which also destroys the dialog!
               widgetDestroy window
    
    case model of
         AiChess _ _ -> forkIO $ forever $ do --handles AI moves
             state <- readIORef stateRef
             if isYourTurn $ currentModel state then putStr "" --do nothing, return (), when, unless freeze program
                                                else do
                                                    let stateAiMove = case getAiMove 3 True (getState (currentModel state)) of
                                                                           Just move -> state { currentModel = executeMove move (currentModel state) }
                                                                           Nothing -> state --maybe add error message
                                                    putStrLn $ show $ getState $ currentModel stateAiMove
                                                    writeIORef stateRef stateAiMove
                                                    postGUIAsync $ widgetQueueDraw canvas
         NetworkChess _ player sock -> forkIO $ forever $ do
             clientIn <- recv sock 4096
             
             if C.null clientIn 
                then putStr ""
                else do 
                    putStr "received: "
                    C.putStrLn clientIn
                    
                    let clientInString = C.unpack clientIn
                        command = head $ words clientInString
                        argument = concat $ tail $ words clientInString
                        
                    case command of
                         --opponent joined game
                         "start" -> do
                             modifyIORef stateRef $ \state -> state { selectedCell = Nothing, currentModel = newNetworkChess Running player sock }
                             postGUIAsync $ widgetQueueDraw canvas
                         --opponent made move
                         "move" -> do
                             let move = read argument::Move
                             
                             putStrLn $ "Opponent made move: " ++ show move
                             modifyIORef stateRef $ \state -> state { selectedCell = Nothing, currentModel = executeMove move (currentModel state) }
                             postGUIAsync $ widgetQueueDraw canvas
                         --opponent quit game/disconnected             
                         "quit" -> postGUISync $ do
                             dialog <- messageDialogNew (Just window) [DialogDestroyWithParent, DialogModal] MessageError ButtonsClose "Opponent disconnected!"
                             set dialog [windowTitle := "Disconnect"]
                             result <- dialogRun dialog
                             --action to be taken after clicked dialog, in this case destroy the window, which also destroys the dialog!
                             widgetDestroy window
                         _ -> putStr command --either unhandled command or updateLobby (which isn't relevant when in game)
         _ -> forkIO $ return () --do nothing           
               
    widgetShowAll window
    on window objectDestroy mainQuit
    mainGUI
    
drawPiece :: Piece -> Cell -> Int -> Render ()
drawPiece piece (row, col) cellSize = do
    piecesvg <- liftIO $ svgNewFromFile $ getPath piece
    let (width, height) = svgGetSize piecesvg
    save
    translate (fromIntegral (col*cellSize)) (fromIntegral ((7-row)*cellSize))
    scale ((fromIntegral cellSize) / (fromIntegral width)) ((fromIntegral cellSize) / (fromIntegral height))
    svgRender piecesvg
    restore

getPath :: Piece -> FilePath
getPath (Piece White pieceType) = case pieceType of
                                       Pawn -> "src/View/pieces/white_pawn.svg"
                                       Knight -> "src/View/pieces/white_knight.svg"
                                       Bishop -> "src/View/pieces/white_bishop.svg"
                                       Rook -> "src/View/pieces/white_rook.svg"
                                       Queen -> "src/View/pieces/white_queen.svg"
                                       King -> "src/View/pieces/white_king.svg"
getPath (Piece Black pieceType) = case pieceType of
                                       Pawn -> "src/View/pieces/black_pawn.svg"
                                       Knight -> "src/View/pieces/black_knight.svg"
                                       Bishop -> "src/View/pieces/black_bishop.svg"
                                       Rook -> "src/View/pieces/black_rook.svg"
                                       Queen -> "src/View/pieces/black_queen.svg"
                                       King -> "src/View/pieces/black_king.svg"
                                
drawPieces :: GameField -> Int -> Render ()
drawPieces field cellSize = forM_ (assocs field) $ \(cell, mpiece) ->
    case mpiece of
         Just piece -> drawPiece piece cell cellSize
         Nothing -> return ()
                                
drawCircle :: Cell -> Int -> Render ()
drawCircle (row, col) cellSize = do
    arc (fromIntegral (col*cellSize)+(fromIntegral cellSize)/2) (fromIntegral ((7-row)*cellSize)+(fromIntegral cellSize)/2) ((fromIntegral cellSize)/4) 0 (2 * pi)
    fill
    
drawSquare :: Cell -> Int -> Render ()
drawSquare (row,col) cellSize = do
    rectangle (fromIntegral (col*cellSize)) (fromIntegral ((7-row)*cellSize)) (fromIntegral cellSize) (fromIntegral cellSize)
    fill
    
drawBoard :: Int -> Render ()
drawBoard cellSize = do
    forM_ [0..7] (\i -> row i)
    where 
        row i = if even i 
                   then do
                       setSourceRGB 0.71 0.53 0.39
                       forM_ [0..3] (\x -> drawSquare ((x*2),i) cellSize)
                       setSourceRGB 0.94 0.85 0.71
                       forM_ [0..3] (\x -> drawSquare (1+x*2,i) cellSize)
                   else do
                       setSourceRGB 0.94 0.85 0.71
                       forM_ [0..3] (\x -> drawSquare ((x*2),i) cellSize)
                       setSourceRGB 0.71 0.53 0.39
                       forM_ [0..3] (\x -> drawSquare (1+x*2,i) cellSize)

drawMoves :: [Move] -> Int -> Render ()
drawMoves moves cellSize = forM_ moves (\move -> drawSquare (getTargetCell move) cellSize)

makeMove :: IORef GUIState -> Move -> IO ()
makeMove stateRef move = do
    state <- readIORef stateRef
    case currentModel state of 
         NetworkChess _ _ sock -> do
             sendMessage sock ("move " ++ show move)
             modifyIORef stateRef $ \state -> state { selectedCell = Nothing, currentModel = executeMove move (currentModel state) }
         _ -> modifyIORef stateRef $ \state -> state { selectedCell = Nothing, currentModel = executeMove move (currentModel state) }
