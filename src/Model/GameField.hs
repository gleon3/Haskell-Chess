module Model.GameField where

import Data.Array

import Model.Cell
import Model.Piece
import Model.Player
import Model.Piece

--(reihe, spalte)
type GameField = Array Cell (Maybe Piece)

size = 8

--prints GameField human-friendly
toString :: GameField -> String
toString field = unlines (zipWith addLineNumber [8,7..1] [unwords [fromMaybe (field ! (row, col)) | col <- [0..7]] | row <- [7,6..0]]) ++ "  a b c d e f g h"
    where
        fromMaybe Nothing = "."
        fromMaybe (Just piece) = show piece
        addLineNumber nr a = show nr ++ " " ++ a

get :: Cell -> GameField -> Maybe Piece
get cell field = if not (isWithinBounds cell) then Nothing
                                              else field ! cell

set :: Cell -> Maybe Piece -> GameField -> GameField
set cell piece field = if not (isWithinBounds cell) then field
                                                    else field // [(cell, piece)]

remove :: Cell -> GameField -> GameField
remove cell field = set cell Nothing field

isCellOfPlayer :: Cell -> Player -> GameField -> Bool
isCellOfPlayer cell player field = case (get cell field) of
                                        Nothing -> False
                                        Just piece -> if (getPlayer piece == player) then True
                                                                                     else False

getCellOfPiece :: Piece -> GameField -> Maybe Cell
getCellOfPiece piece field | x == [] = Nothing
                           | otherwise = Just (head x)
    where x = [ i | (i, pc) <- (assocs $ field), pc == Just piece]
                                                                                     
getCellsOfPlayer :: Player -> GameField -> [Cell]
getCellsOfPlayer player field = [ i | (i, piece) <- (assocs $ field), isCellOfPlayer i player field]

setInitialPieces :: GameField
setInitialPieces = emptyField // (row0++row1++row6++row7)
    where
        row0 = rowList 0 White [Rook, Knight, Bishop, King, Queen, Bishop, Knight, Rook]
        row1 = rowList 1 White (replicate 8 Pawn)
        row6 = rowList 6 Black (replicate 8 Pawn)
        row7 = rowList 7 Black [Rook, Knight, Bishop, King, Queen, Bishop, Knight, Rook]
        rowList row player list = zip (range ((row,0),(row,7))) (map(\t->Just(Piece player t)) list)
        emptyField = array bound [(i,Nothing)|i<-range bound]
        bound = ((0,0),(size-1, size-1))
                                                 
isWithinBounds :: Cell -> Bool
isWithinBounds (col, row) = row >= 0 && row < size && col >= 0 && col < size
