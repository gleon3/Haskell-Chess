module View.ChessView where


import Graphics.Rendering.Cairo
import Graphics.Rendering.Cairo.SVG
import Graphics.UI.Gtk hiding (rectangle,get)

import Model.Model
import Model.AiChess
import Model.GameState
import Model.GameField hiding (set)
import Model.Phase
import Model.Move
import Model.Cell
import Model.Piece
import Model.Player
import Control.Monad

import Data.Array
import Data.List
import Data.IORef

data GUIState = GUIState { selectedCell :: Maybe Cell, currentModel :: Model }

initGUIState :: Model -> IO (IORef GUIState)
initGUIState (Chess _) = newIORef $ GUIState { selectedCell = Nothing, currentModel = newChess Running }
initGUIState (AiChess _ player) = newIORef $ GUIState { selectedCell = Nothing, currentModel = newAiChess Running player}
initGUIState (NetworkChess _ player) = newIORef $ GUIState { selectedCell = Nothing, currentModel = newNetworkChess Running player}

setupBoard :: Model -> IO ()
setupBoard model = do
    initGUI
    window <- windowNew
    
    stateRef <- initGUIState model
    
    canvas <- drawingAreaNew
    
    set window [containerBorderWidth := 0, windowTitle := "Chess", containerChild := canvas, windowDefaultHeight := 700, windowDefaultWidth := 700]
    
    on canvas draw $ do
        width <- liftIO $ widgetGetAllocatedWidth canvas
        height <- liftIO $ widgetGetAllocatedHeight canvas
        
        let cellSize' = min ((fromIntegral width)/(fromIntegral size)) ((fromIntegral height)/(fromIntegral size))
            cellSize = round cellSize'
        
        drawBoard cellSize
                    
        liftIO (readIORef stateRef) >>= \GUIState { selectedCell = currentSelected, currentModel = curModel } -> do
            --draw selected cell and possible moves from given cell, only if your turn!
            if isYourTurn curModel 
               then case currentSelected of 
                         Just cell -> do
                             setSourceRGB 0.4 0.4 0.25
                             drawSquare cell cellSize
                             setSourceRGB 0.51 0.6 0.41
                             drawMoves (getPossibleMovesForPiece cell curModel) cellSize
                         Nothing -> return ()              
               else return ()
                        
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
                    

            if not $ isYourTurn curModel 
               then do 
                   liftIO $ modifyIORef stateRef $ \state -> case (currentModel state) of 
                                                                  AiChess _ _ -> case getAiMove 3 True (getState (currentModel state)) of
                                                                                      Just move -> state { selectedCell = Nothing, currentModel = executeMove move (currentModel state) }
                                                                                      Nothing -> state --maybe add error message
                                                                  _ -> state
                   liftIO $ widgetQueueDraw canvas 
               else return ()
            
                                                                
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
                   
                   case currentSelected of
                        Nothing ->  if get cell (gameField $ getState curModel) == Nothing
                                    then liftIO $ modifyIORef stateRef $ \state -> state { selectedCell = Nothing }
                                    else liftIO $ modifyIORef stateRef $ \state -> state { selectedCell = Just cell }
                        Just selected -> case find (\move -> getTargetCell move == cell) (getPossibleMovesForPiece selected curModel) of
                                            Just (PawnPromotion from to _) -> do
                                                dialog <- liftIO $ messageDialogNew (Just window) [DialogDestroyWithParent, DialogModal] MessageQuestion ButtonsNone "Which piece do you want to promote to?"
                                                liftIO $ dialogAddButton dialog "Queen" (ResponseUser 0)
                                                liftIO $ dialogAddButton dialog "Rook" (ResponseUser 1)
                                                liftIO $ dialogAddButton dialog "Bishop" (ResponseUser 2)
                                                liftIO $ dialogAddButton dialog "Knight" (ResponseUser 3)
                                                liftIO $ set dialog [windowTitle := "PawnPromotion"]
                                                result <- liftIO $ dialogRun dialog
                                                liftIO $ widgetDestroy dialog
                                                case result of
                                                   ResponseUser 0 -> liftIO $ modifyIORef stateRef $ 
                                                       \state -> state { selectedCell = Nothing, currentModel = executeMove (PawnPromotion from to Queen) (currentModel state) }
                                                   ResponseUser 1 -> liftIO $ modifyIORef stateRef $ 
                                                       \state -> state { selectedCell = Nothing, currentModel = executeMove (PawnPromotion from to Rook) (currentModel state) }
                                                   ResponseUser 2 -> liftIO $ modifyIORef stateRef $ 
                                                       \state -> state { selectedCell = Nothing, currentModel = executeMove (PawnPromotion from to Bishop) (currentModel state) }
                                                   ResponseUser 3 -> liftIO $ modifyIORef stateRef $ 
                                                       \state -> state { selectedCell = Nothing, currentModel = executeMove (PawnPromotion from to Knight) (currentModel state) }
                                                   ResponseUser _ -> error "unhandled response"
                                                   _ -> liftIO $ modifyIORef stateRef $ \state -> state { currentModel = executeMove (PawnPromotion from to Queen) (currentModel state), selectedCell = Nothing }
                                            Just move -> liftIO $ modifyIORef stateRef $ \state -> state { selectedCell = Nothing, currentModel = executeMove move (currentModel state) }
                                            _ -> if get cell (gameField $ getState curModel) == Nothing
                                                    then liftIO $ modifyIORef stateRef $ \state -> state { selectedCell = Nothing }
                                                    else liftIO $ modifyIORef stateRef $ \state -> state { selectedCell = Just cell }
                   
                   liftIO $ widgetQueueDraw canvas
                   
                   --read new state and check if game ended after making move and give dialog
                   liftIO (readIORef stateRef) >>= \GUIState { selectedCell = newCurrentSelected, currentModel = newCurModel } -> 
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
                           else return ()
                   
                   return True
               else return False
        
    --on canvas motionNotifyEvent $ do
    --    maybe add hoveredCell
    
    widgetShowAll window
    on window objectDestroy mainQuit
    mainGUI
    
test :: a -> IO () -> Int
test a b = 3
    
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

drawSquareOutline :: Cell -> Int -> Render ()
drawSquareOutline (row,col) cellSize = do
    rectangle (fromIntegral (col*cellSize)) (fromIntegral ((7-row)*cellSize)) (fromIntegral cellSize) (fromIntegral cellSize)
    stroke
    
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
