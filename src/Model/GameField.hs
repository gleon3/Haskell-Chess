module Model.GameField where

import Data.Array

import Model.Cell
import Model.Piece
import Model.Player

--(reihe, spalte)
type GameField = Array Cell (Maybe Piece)

size = 8

getBaseRowIndex :: Player -> Int
getBaseRowIndex White = 0
getBaseRowIndex Black = 7

getDirection :: Player -> Int
getDirection White = 1
getDirection Black = -1

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

amountOfPiece :: Piece -> GameField -> Int
amountOfPiece piece field = length [ pc | (i, pc) <- (assocs $ field), pc == Just piece]

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
getCellsOfPlayer player field = [ cell | (cell, piece) <- (assocs $ field), isCellOfPlayer cell player field]

getPiecesOfPlayer :: Player -> GameField -> [Piece]
getPiecesOfPlayer player field = [ piece | (cell, Just piece) <- assocs $ field, isCellOfPlayer cell player field]

setInitialPieces :: GameField
setInitialPieces = emptyField // (row0++row1++row6++row7)
    where
        row0 = rowList 0 White [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]
        row1 = rowList 1 White (replicate 8 Pawn)
        row6 = rowList 6 Black (replicate 8 Pawn)
        row7 = rowList 7 Black [Rook, Knight, Bishop, Queen, King , Bishop, Knight, Rook]
        rowList row player list = zip (range ((row,0),(row,7))) (map(\t->Just(Piece player t)) list)
        emptyField = array bound [(i,Nothing)|i<-range bound]
        bound = ((0,0),(size-1, size-1))
        
--removes piece from initial square and sets it to target square
movePiece :: Cell -> Cell -> GameField -> GameField
movePiece from to field = set to (get from field) $ remove from field
                                                 
isWithinBounds :: Cell -> Bool
isWithinBounds (row, col) = row >= 0 && row < size && col >= 0 && col < size
