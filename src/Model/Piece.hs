module Model.Piece (Piece, PieceType, getPlayer, getPieceType) where

import Data.Char

import Model.Player

data PieceType = Pawn
               | Rook
               | Knight 
               | Bishop
               | Queen
               | King
               deriving (Eq,Read)
               
instance Show PieceType where
    show Pawn = "P"
    show Rook = "R"
    show Knight = "N"
    show Bishop = "B"
    show Queen = "Q"
    show King = "K"
  
data Piece = Piece Player PieceType deriving (Eq)
    
instance Show Piece where
    show (Piece White pieceType) = show pieceType
    show (Piece Black pieceType) = map toLower (show pieceType)
    
getPlayer :: Piece -> Player
getPlayer (Piece p _) = p

getPieceType :: Piece -> PieceType
getPieceType (Piece _ t) = t
