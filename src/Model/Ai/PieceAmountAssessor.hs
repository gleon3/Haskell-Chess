module Model.Ai.PieceAmountAssessor where

import Model.GameField
import Model.GameState
import Model.Piece
import Model.Player

computeValue :: GameState -> Int
computeValue state = 10 * (amountOfPiece (Piece White Pawn) (gameField state) - amountOfPiece (Piece Black Pawn) (gameField state))
                   + 30 * (amountOfPiece (Piece White Knight) (gameField state) - amountOfPiece (Piece Black Knight) (gameField state))
                   + 30 * (amountOfPiece (Piece White Bishop) (gameField state) - amountOfPiece (Piece Black Bishop) (gameField state))
                   + 50 * (amountOfPiece (Piece White Rook) (gameField state) - amountOfPiece (Piece Black Rook) (gameField state))
                   + 90 * (amountOfPiece (Piece White Queen) (gameField state) - amountOfPiece (Piece Black Queen) (gameField state))
