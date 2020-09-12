module Model.Ai.WinAssessor where

import Model.Chess
import Model.GameState
import Model.Player

computeValue :: GameState -> Int
computeValue state | isCheckmate White state = -2000
                   | isCheckmate Black state = 2000
                   | otherwise = 0
