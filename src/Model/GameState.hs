module Model.GameState ( getTypeOfMove ) where

import Model.Cell
import Model.Move
import Model.Player
import Model.Phase
import Model.GameField

data GameState = GameState { currentPlayer :: Player, gameField :: GameField, currentPhase :: Phase, winner :: Maybe Player, lastMovePawn :: Maybe Cell, castlingFlags :: String } deriving (Eq)

getTypeOfMove :: Cell -> Cell -> GameState -> MoveType 
getTypeOfMove from to state = "TODO"
