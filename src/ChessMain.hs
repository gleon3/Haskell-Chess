module ChessMain ( startGUI ) where
        
import Graphics.UI.Gtk

startGUI :: IO ()
startGUI =  do
    initGUI
    window <- windowNew
    button <- buttonNew
    set window [ containerBorderWidth := 10, containerChild := button ]
    set button [ buttonLabel := "Hello World" ]
    on button buttonActivated (callback1)
    on window objectDestroy mainQuit
    widgetShowAll window
    mainGUI
    
callback1 :: IO ()
callback1 = putStrLn "Hello World"
