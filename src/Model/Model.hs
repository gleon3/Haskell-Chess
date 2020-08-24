module Model.Model where

import Model.Move
import Model.Cell
import Model.GameState
import Model.Player 

data Model = Chess GameState
           | AiChess GameState Player
           | NetworkChess GameState Player

getState :: Model -> GameState
getState (Chess state) = state
getState (AiChess state _) = state
getState (NetworkChess state _) = state

getPossibleMovesForPiece :: Cell -> Model -> [Move]
getPossibleMovesForPiece from model = []

move :: Move -> Model -> Model
move move model = model
