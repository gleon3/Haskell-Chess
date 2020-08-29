module View.ChessView where


import Graphics.Rendering.Cairo
import Graphics.UI.Gtk hiding (rectangle)

import Model.Model
import Model.GameState
import Model.Phase
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
    
    ebox <- eventBoxNew
    vbox <- vBoxNew True 0
    widgetModifyBg vbox StateNormal (Color 40000 40000 40000)
    
    containerAdd ebox vbox
    
    widgetSetAppPaintable window True
    
    set window [containerBorderWidth := 0, windowTitle := "Chess", containerChild := ebox, windowDefaultHeight := 700, windowDefaultWidth := 700]
    
    player <- labelNew (Just $ show $ currentPlayer (getState model))
    
    containerAdd vbox player
    
    canvas <- drawingAreaNew
    containerAdd vbox canvas
    
    on canvas draw $ do
        drawBoard

    --quitButton <- statusbarNew
    
   -- containerAdd vbox quitButton
    
    on window objectDestroy mainQuit
    widgetShowAll window
    mainGUI
    
drawSquare :: Int -> Int -> Bool -> Render ()
drawSquare i j light = do
    if light then setSourceRGB 1.0 1.0 1.0
             else setSourceRGB 0.0 0.0 0.0
    rectangle (fromIntegral (paddingx+i*cellSize)) (fromIntegral (paddingy+j*cellSize)) (fromIntegral cellSize) (fromIntegral cellSize)
    fill
    
drawBoard :: Render ()
drawBoard = do
    forM_ [0..7] (\i -> row i)
    where 
        row i = if even i then forM_ [0..3] (\x -> drawSquare (x*2) i True) >> forM_ [0..3] (\x -> drawSquare (1+x*2) i False)
                          else forM_ [0..3] (\x -> drawSquare (x*2) i False) >> forM_ [0..3] (\x -> drawSquare (1+x*2) i True)
            
