module Model.Chess where

import Model.GameState
import Model.GameField
import Model.Move
import Model.Player
import Model.Cell
import Model.Piece
import Model.Phase

data MoveError = InvalidMove
               | NoGameRunning
               | NoPiece
               | OutOfBounds 
               | NotYourTurn
               deriving (Eq, Show)

--initializes a new chess game
newGame :: Phase -> GameState
newGame startingPhase = GameState { currentPlayer = White, gameField = setInitialPieces, currentPhase = startingPhase, winner = Nothing, lastDoubleStep = Nothing, castlingFlags = "KQkq" }

canMove :: Player -> GameState -> Bool
canMove player state = if getLegalMovesForPlayer player state == [] then False
                                                                    else True
isCheckmate :: Player -> GameState -> Bool
isCheckmate player state = isChecked player state && (not $ canMove player state)

isStalemate :: Player -> GameState -> Bool
isStalemate player state = (not $ isChecked player state) && (not $ canMove player state)

--checks validity of move, if it is valid perform move, otherwise give error
move :: Move -> GameState -> Either GameState MoveError
move move state | currentPhase state /= Running = Right NoGameRunning
                | not $ isWithinBounds from && isWithinBounds to = Right OutOfBounds
                | not $ elem move $ getPossibleMovesForPiece from state = Right InvalidMove
                | otherwise = case get from (gameField state) of
                                   Just (Piece player _) -> Left (executeMove move state)
                                   _ -> Right NoPiece
    where from = getSourceCell move
          to = getTargetCell move
          player = currentPlayer state
          
--checks for winning condition, resets lastDoubleStep, swap current player
nextTurn :: GameState -> GameState
nextTurn state | isCheckmate opponent state = state { winner = Just (currentPlayer state), currentPhase = Finished } 
               | isStalemate opponent state = state { winner = Nothing, currentPhase = Finished } 
               | otherwise = state { currentPlayer = opponent, lastDoubleStep = Nothing }
    where opponent = getOpponentOf (currentPlayer state)
          
--removes piece from initial square and sets it to target square
executeNormalMove :: Cell -> Cell -> GameState -> GameState
executeNormalMove from to state = nextTurn $ state { gameField = set to (get from (gameField state)) $ remove from (gameField state) }
          
--executes move, doesnt check if valid
executeMove :: Move -> GameState -> GameState 
executeMove (Move from to) state 
    | getRow from == getBaseRowIndex (currentPlayer state) && get from (gameField state) == Just (Piece (currentPlayer state) Rook) = 
        case getColumn to of
             0 -> removeCastle False (currentPlayer state) $ executeNormalMove from to state
             7 -> removeCastle True (currentPlayer state) $ executeNormalMove from to state
             _ -> executeNormalMove from to state
    | otherwise = executeNormalMove from to state
executeMove (DoubleStepMove from to) state = (executeNormalMove from to state) { lastDoubleStep = Just to }
executeMove (EnPassant from to lastDoubleStep) state = (executeNormalMove from to state) { gameField = remove lastDoubleStep (gameField state) }
executeMove (PawnPromotion from to pieceType) state = nextTurn $ state { gameField = set to (Just $ Piece (currentPlayer state) pieceType) $ remove from (gameField state) }
executeMove (Castle kingside) state | kingside = removeCastle False (currentPlayer state) $ removeCastle True (currentPlayer state) $ rookMoveKingside $ kingMoveKingside state
                                    | otherwise = removeCastle False (currentPlayer state) $ removeCastle True (currentPlayer state) $ rookMoveQueenside $ kingMoveQueenside state
    where kingMoveKingside state = executeNormalMove (getBaseRowIndex (currentPlayer state), 4) (getBaseRowIndex (currentPlayer state), 6) state
          rookMoveKingside state = executeNormalMove (getBaseRowIndex (currentPlayer state), 7) (getBaseRowIndex (currentPlayer state), 5) state
          kingMoveQueenside state = executeNormalMove (getBaseRowIndex (currentPlayer state), 4) (getBaseRowIndex (currentPlayer state), 2) state
          rookMoveQueenside state = executeNormalMove (getBaseRowIndex (currentPlayer state), 0) (getBaseRowIndex (currentPlayer state), 3) state
          
getLegalMovesForPlayer :: Player -> GameState -> [Move]
getLegalMovesForPlayer player state = [possibleMove | from <- getCellsOfPlayer player (gameField state), possibleMove <- getPossibleMovesForPiece from state]
          
getPossibleMovesForPiece :: Cell -> GameState -> [Move]
getPossibleMovesForPiece from state | currentPhase state /= Running = [] --when game is not running, there shouldn't be moves possible 
                                    | otherwise =
                                        case get from (gameField state) of
                                             Just (Piece player King) -> getPossibleMovesForPieceNC from player state ++ getCastlingMoves player state --TODO: only if not put  in check
                                             Just (Piece player _) -> if isChecked player state then [] --if checked only king is moveable
                                                                                                else getPossibleMovesForPieceNC from player state --TODO: only if not put in check
                                             _ -> [] --when given cell is not of player, there shouldn't be moves possible
    where player = currentPlayer state
