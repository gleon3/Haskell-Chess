module Model.Cell ( Cell, getRow, getColumn, formatCell, parseCell, parseRowValue, parseColumnValue, checkValidCellFormat ) where

import Data.Char

type Cell = (Int, Int)

getRow :: Cell -> Int
getRow (row, _) = row

getColumn :: Cell -> Int
getColumn (row, col) = col

formatCell :: Cell -> String
formatCell (row, col) = [chr (97+col), (intToDigit (row + 1))]

parseCell :: String -> Cell
parseCell s = (parseRowValue s, parseColumnValue s)

--A5 -> 4
parseRowValue :: String -> Int
parseRowValue s = digitToInt row - 1
    where row = head (tail s)

--A1 -> 0
parseColumnValue :: String -> Int   
parseColumnValue s = if (isUpper col) then ord col - 65
                                      else ord col - 97
    where col = head s

--has to be A1    
checkValidCellFormat :: String -> Bool
checkValidCellFormat s = length s == 2 && isAlpha (head s) && isDigit (head (tail s))
