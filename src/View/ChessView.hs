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
    --TODO: handle game ended! handle pawn promotion
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
                             setSourceRGB 0.0 0.0 0.75
                             drawSquare cell cellSize
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
                    
            --if game finished show it to player!
            if (currentPhase $ getState curModel) == Finished then liftIO $ putStrLn "TODO: do end game dialog"
                                                              else return ()
            if not $ isYourTurn curModel 
               then do 
                   liftIO $ modifyIORef stateRef $ \state -> case (currentModel state) of 
                                                                  AiChess _ _ -> case getAiMove 3 True (getState (currentModel state)) of
                                                                                      Just move -> state { selectedCell = Nothing, currentModel = executeMove move (currentModel state) }
                                                                                      Nothing -> state 
                                                                  _ -> state
                   liftIO $ widgetQueueDraw canvas 
               else return ()
            
                                                                
    on canvas buttonPressEvent $ do
        width <- liftIO $ widgetGetAllocatedWidth canvas
        height <- liftIO $ widgetGetAllocatedHeight canvas
        
        let cellSize' = min ((fromIntegral width)/(fromIntegral size)) ((fromIntegral height)/(fromIntegral size))
            cellSize = round cellSize'
        
        button <- eventButton
        (x,y) <- eventCoordinates
        
        let cell = (7-(floor $ y/(fromIntegral cellSize)),(floor $ x/(fromIntegral cellSize)))
        
        liftIO $ modifyIORef stateRef $ \state -> case selectedCell state of
                                                       Nothing -> if get cell (gameField $ getState (currentModel state)) == Nothing
                                                                     then state { selectedCell = Nothing }
                                                                     else state { selectedCell = Just cell }
                                                       Just selected -> case find (\move -> getTargetCell move == cell) (getPossibleMovesForPiece selected (currentModel state)) of 
                                                                             Nothing -> if get cell (gameField $ getState (currentModel state)) == Nothing
                                                                                           then state { selectedCell = Nothing }
                                                                                           else state { selectedCell = Just cell }
                                                                             Just move -> state { selectedCell = Nothing, currentModel = executeMove move (currentModel state) }
                                                                             
        liftIO $ widgetQueueDraw canvas
        return False
    
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
    setSourceRGB 1.0 1.0 1.0
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
drawMoves moves cellSize = forM_ moves (\move -> drawCircle (getTargetCell move) cellSize)
