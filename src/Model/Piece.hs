module Model.Piece where

import Data.Char

import Model.Player

data PieceType = Pawn
               | Rook
               | Knight 
               | Bishop
               | Queen
               | King
               deriving (Eq,Show,Read)

data Piece = Piece Player PieceType deriving (Eq)
    
instance Show Piece where
    show (Piece White pieceType) = typeToString pieceType
    show (Piece Black pieceType) = map toLower (typeToString pieceType)
    
typeToString :: PieceType -> String
typeToString Pawn = "P"
typeToString Rook = "R"
typeToString Knight = "N"
typeToString Bishop = "B"
typeToString Queen = "Q"
typeToString King = "K"
    
getPlayer :: Piece -> Player
getPlayer (Piece p _) = p

getPieceType :: Piece -> PieceType
getPieceType (Piece _ t) = t
