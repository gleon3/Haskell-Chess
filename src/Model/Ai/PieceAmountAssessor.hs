module Model.Ai.PieceAmountAssessor where

import Model.GameState
import Model.GameField
import Model.Piece
import Model.Player
import Model.Chess

computeValue :: GameState -> Float
computeValue state | isCheckmate (currentPlayer state) state = -20000
                   | isStalemate (currentPlayer state) state = 0
                   | otherwise = 100 * fromIntegral (amountOfPiece (Piece White Pawn) (gameField state) - amountOfPiece (Piece Black Pawn) (gameField state)) 
                   + 300 * fromIntegral (amountOfPiece (Piece White Knight) (gameField state) - amountOfPiece (Piece Black Knight) (gameField state))
                   + 300 * fromIntegral (amountOfPiece (Piece White Bishop) (gameField state) - amountOfPiece (Piece Black Bishop) (gameField state))
                   + 500 * fromIntegral (amountOfPiece (Piece White Rook) (gameField state) - amountOfPiece (Piece Black Rook) (gameField state))
                   + 900 * fromIntegral (amountOfPiece (Piece White Queen) (gameField state) - amountOfPiece (Piece Black Queen) (gameField state))
