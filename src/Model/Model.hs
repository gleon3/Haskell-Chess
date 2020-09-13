module Model.Model where

import Model.Cell
import Model.GameState
import Model.Move
import Model.Phase
import Model.Player

import Network.Socket

import Model.Chess as Chess

data Model = Chess GameState
           | AiChess GameState Player
           | NetworkChess GameState Player Socket
  
newChess :: Phase -> Model
newChess phase = Chess (newGame phase)

newAiChess :: Phase -> Player -> Model
newAiChess phase player = AiChess (newGame phase) player

newNetworkChess :: Phase -> Player -> (Socket -> Model)
newNetworkChess phase player = NetworkChess (newGame phase) player --socket will be defined in io

getState :: Model -> GameState
getState (Chess state) = state
getState (AiChess state _) = state
getState (NetworkChess state _ _) = state

setState :: GameState -> Model -> Model
setState newState (Chess state) = Chess newState
setState newState (AiChess state player) = AiChess newState player
setState newState (NetworkChess state player socket) = NetworkChess newState player socket

isYourTurn :: Model -> Bool
isYourTurn (AiChess state player) = if currentPlayer state == player then True
                                                                     else False
isYourTurn (NetworkChess state player _) = if currentPlayer state == player then True
                                                                            else False
isYourTurn _ = True

getPossibleMovesForPiece :: Cell -> Model -> [Move]
getPossibleMovesForPiece from model = if isYourTurn model then Chess.getPossibleMovesForPiece (currentPlayer state) from state
                                                          else []
    where state = getState model

getLegalMoves :: Model -> [Move]
getLegalMoves model = if isYourTurn model then Chess.getLegalMovesForPlayer (currentPlayer state) state
                                          else []
    where state = getState model

move :: Move -> Model -> Either Model MoveError
move move model | isYourTurn model = case Chess.move move (getState model) of
                                          Left newState -> Left (setState newState model)
                                          Right err -> Right err
                | otherwise = Right NotYourTurn

executeMove :: Move -> Model -> Model
executeMove move model = setState (Chess.performMove move (getState model)) model
