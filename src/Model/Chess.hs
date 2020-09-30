module Model.Chess where

import Model.Cell
import Model.GameField
import Model.GameState
import Model.Phase
import Model.Piece
import Model.Player
import Model.Move

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
canMove player state = not $ getLegalMovesForPlayer player state == []

isCheckmate :: Player -> GameState -> Bool
isCheckmate player state = isChecked player state && (not $ canMove player state)

isStalemate :: Player -> GameState -> Bool
isStalemate player state = (not $ isChecked player state) && (not $ canMove player state)

--not enough material for a checkmate to occur
insufficentMaterial :: GameState -> Bool
insufficentMaterial state = (onlyKing White || onlyKingBishop White || onlyKingKnight White || onlyKing2Knight White) 
                            && (onlyKing Black || onlyKingBishop Black || onlyKingKnight Black || onlyKing2Knight Black)
    where pieces player = getPiecesOfPlayer player (gameField state)
          onlyKing player = pieces player == [Piece player King]
          onlyKingBishop player = (length (pieces player) == 2) && (elem (Piece player King) (pieces player)) && (elem (Piece player Bishop) (pieces player))
          onlyKingKnight player = (length (pieces player) == 2) && (elem (Piece player King) (pieces player)) && (elem (Piece player Knight) (pieces player))
          onlyKing2Knight player = (length (pieces player) == 3) && (elem (Piece player King) (pieces player)) && (amountOfPiece (Piece player Knight) (gameField state) == 2)

--checks validity of move, if it is valid perform move, otherwise give error
move :: Move -> GameState -> Either GameState MoveError
move move state | currentPhase state /= Running = Right NoGameRunning
                | not $ isWithinBounds from && isWithinBounds to = Right OutOfBounds
                | not $ elem move $ getPossibleMovesForPiece (currentPlayer state) from state = Right InvalidMove
                | otherwise = case get from (gameField state) of
                                   Just (Piece player _) -> Left (performMove move state)
                                   _ -> Right NoPiece
    where from = getSourceCell move
          to = getTargetCell move
          player = currentPlayer state
          
--checks for winning condition, resets lastDoubleStep, swap current player
nextTurn :: GameState -> GameState
nextTurn state | isCheckmate opponent state = state { currentPlayer = opponent, winner = Just (currentPlayer state), currentPhase = Finished } 
               | isStalemate opponent state = state { currentPlayer = opponent, winner = Nothing, currentPhase = Finished } 
               | insufficentMaterial state = state { currentPlayer = opponent, winner = Nothing, currentPhase = Finished }
               | otherwise = state { currentPlayer = opponent, lastDoubleStep = Nothing }
    where opponent = getOpponentOf (currentPlayer state)

--performs move, doesnt check if valid, change to next turn
performMove :: Move -> GameState -> GameState
performMove (DoubleStepMove from to) state = (nextTurn $ executeMove (DoubleStepMove from to) state) { lastDoubleStep = Just to }
performMove move state = nextTurn $ executeMove move state
    
movePieceNormal :: Cell -> Cell -> GameState -> GameState
movePieceNormal from to state = state { gameField = movePiece from to (gameField state) }  

--executes move, doesnt check if valid, doesn't change to next turn
executeMove :: Move -> GameState -> GameState 
executeMove (Move from to) state 
    | getRow from == getBaseRowIndex (currentPlayer state) && get from (gameField state) == Just (Piece (currentPlayer state) Rook) = 
        case getColumn to of
             0 -> removeCastle False (currentPlayer state) $ movePieceNormal from to state
             7 -> removeCastle True (currentPlayer state) $ movePieceNormal from to state
             _ -> movePieceNormal from to state
    | getRow from == getBaseRowIndex (currentPlayer state) && get from (gameField state) == Just (Piece (currentPlayer state) King) && getColumn from == 4 
    = removeCastle False (currentPlayer state) $ removeCastle True (currentPlayer state) $ movePieceNormal from to state
    | otherwise = movePieceNormal from to state
executeMove (DoubleStepMove from to) state = movePieceNormal from to state
executeMove (EnPassant from to lastDoubleStep) state = state { gameField = remove lastDoubleStep (gameField $ movePieceNormal from to state ) }
executeMove (PawnPromotion from to pieceType) state = state { gameField = set to (Just $ Piece (currentPlayer state) pieceType) $ remove from (gameField state) }
executeMove (Castle kingside _) state | kingside = removeCastle False (currentPlayer state) $ removeCastle True (currentPlayer state) $ rookMoveKingside $ kingMoveKingside state
                                      | otherwise = removeCastle False (currentPlayer state) $ removeCastle True (currentPlayer state) $ rookMoveQueenside $ kingMoveQueenside state
    where 
        kingMoveKingside state = movePieceNormal (getBaseRowIndex (currentPlayer state), 4) (getBaseRowIndex (currentPlayer state), 6) state
        rookMoveKingside state = movePieceNormal (getBaseRowIndex (currentPlayer state), 7) (getBaseRowIndex (currentPlayer state), 5) state
        kingMoveQueenside state = movePieceNormal (getBaseRowIndex (currentPlayer state), 4) (getBaseRowIndex (currentPlayer state), 2) state
        rookMoveQueenside state = movePieceNormal (getBaseRowIndex (currentPlayer state), 0) (getBaseRowIndex (currentPlayer state), 3) state
          
getLegalMovesForPlayer :: Player -> GameState -> [Move]
getLegalMovesForPlayer player state = [possibleMove | from <- getCellsOfPlayer player (gameField state), possibleMove <- getPossibleMovesForPiece player from state]
          
getPossibleMovesForPiece :: Player -> Cell -> GameState -> [Move]
getPossibleMovesForPiece player from state | currentPhase state /= Running = [] --when game is not running, there shouldn't be moves possible 
                                           | not $ isCellOfPlayer from player (gameField state) = [] --not a piece of currentPlayer on from cell -> obv no moves possible
                                           | otherwise =
                                               case get from (gameField state) of
                                                    Just (Piece _ King) -> let moves = getPossibleMovesForPieceNC from player state ++ getCastlingMoves player state
                                                                           in [x | x <- moves, not $ movePutsInCheck player x state ]
                                                    Just (Piece _ _) -> let moves = getPossibleMovesForPieceNC from player state
                                                                        in [x | x <- moves, not $ movePutsInCheck player x state ]
                                                    _ -> [] --when given cell is not of player, there shouldn't be moves possible
                                                    
movePutsInCheck :: Player -> Move -> GameState -> Bool
movePutsInCheck player move state = isChecked player (executeMove move state)
