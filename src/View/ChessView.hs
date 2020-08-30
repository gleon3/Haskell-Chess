module View.ChessView where


import Graphics.Rendering.Cairo
import Graphics.Rendering.Cairo.SVG
import Graphics.UI.Gtk hiding (rectangle)

import Model.Model
import Model.GameState
import Model.Phase
import Model.Cell
import Model.Piece
import Model.Player
import Control.Monad

paddingx = 50
paddingy = 50
cellSize = 50

showModelNew :: IO ()
showModelNew = showGameState $ newChess Running

showGameState :: Model -> IO ()
showGameState model = do
    initGUI
    window <- windowNew
    
    --player <- builderGetObject builder castToLabel "label1"
    
    canvas <- drawingAreaNew
    set window [containerBorderWidth := 0, windowTitle := "Chess", containerChild := canvas, windowDefaultHeight := 700, windowDefaultWidth := 700]
    on canvas draw $ do
        drawBoard
        drawCircle (0,5)
        drawPiece (Piece White Pawn) (0,0)
        drawPiece (Piece White Knight) (1,0)
        drawPiece (Piece White Bishop) (2,0)
        drawPiece (Piece White Rook) (3,0)
        drawPiece (Piece White Queen) (4,0)
        drawPiece (Piece White King) (5,0)
        drawPiece (Piece White Pawn) (6,0)
        drawPiece (Piece White Pawn) (7,0)
        drawPiece (Piece Black Pawn) (0,1)
        drawPiece (Piece Black Knight) (1,1)
        drawPiece (Piece Black Bishop) (2,1)
        drawPiece (Piece Black Rook) (3,1)
        drawPiece (Piece Black Queen) (4,1)
        drawPiece (Piece Black King) (5,1)
        drawPiece (Piece Black Pawn) (6,1)
        drawPiece (Piece Black Pawn) (7,1)

    --player <- builderGetObject builder castToButton "button1"
    
    widgetShowAll window
    on window objectDestroy mainQuit
    mainGUI
    
drawPiece :: Piece -> Cell -> Render ()
drawPiece piece (row, col) = do
    piecesvg <- liftIO $ svgNewFromFile $ getPath piece
    let (width, height) = svgGetSize piecesvg
    save
    translate (fromIntegral (paddingx+col*cellSize)) (fromIntegral (paddingy+row*cellSize))
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
    
drawCircle :: Cell -> Render ()
drawCircle (row, col) = do
    setSourceRGB 1.0 0.0 0.0
    arc (fromIntegral (paddingx+col*cellSize)+(fromIntegral cellSize)/2) (fromIntegral (paddingy+row*cellSize)+(fromIntegral cellSize)/2) ((fromIntegral cellSize)/4) 0 (2 * pi)
    fill
    
drawSquare :: Cell -> Bool -> Render ()
drawSquare (row,col) light = do
    if light then setSourceRGB 0.71 0.53 0.39
             else setSourceRGB 0.94 0.85 0.71
    rectangle (fromIntegral (paddingx+col*cellSize)) (fromIntegral (paddingy+row*cellSize)) (fromIntegral cellSize) (fromIntegral cellSize)
    fill
    
drawBoard :: Render ()
drawBoard = do
    forM_ [7,6..0] (\i -> row i)
    where 
        row i = if even i then forM_ [0..3] (\x -> drawSquare ((x*2),i) True) >> forM_ [0..3] (\x -> drawSquare (1+x*2,i) False)
                          else forM_ [0..3] (\x -> drawSquare ((x*2),i) False) >> forM_ [0..3] (\x -> drawSquare (1+x*2,i) True)
            
