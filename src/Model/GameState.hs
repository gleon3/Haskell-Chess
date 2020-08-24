module Model.GameState where

import Model.Cell
import Model.Move
import Model.Player
import Model.Phase
import Model.GameField
import Model.Piece

data GameState = GameState { currentPlayer :: Player, gameField :: GameField, currentPhase :: Phase, winner :: Maybe Player, lastDoubleStep :: Maybe Cell, castlingFlags :: String } deriving (Eq)

instance Show GameState
    where 
        show (GameState _ gameField Finished (Just winner) _ _ ) = "Thanks for playing! " ++ "\n" ++ "Player " ++ show winner ++ " has won!\n" ++ toString gameField
        show (GameState _ gameField Finished Nothing _ _ ) = "Thanks for playing! " ++ "\n" ++ "It's a draw!\n" ++ toString gameField  
        show (GameState currentPlayer gameField _ _ _ _) = "player to move: " ++ show currentPlayer ++ "\n" ++ toString gameField

getCurrentPlayer :: GameState -> Player
getCurrentPlayer state = currentPlayer state

setCurrentPlayer :: GameState -> Player -> GameState
setCurrentPlayer state player = state { currentPlayer = player }

getField :: GameState -> GameField
getField state = gameField state

getWinner :: GameState -> Maybe Player
getWinner state = winner state

--Nothing = draw
setWinner :: GameState -> Maybe Player -> GameState
setWinner state winner = state { winner = winner }

getCurrentPhase :: GameState -> Phase
getCurrentPhase state = currentPhase state

setCurrentPhase :: GameState -> Phase -> GameState 
setCurrentPhase state phase = state { currentPhase = phase }

getCastlingFlags :: GameState -> String
getCastlingFlags state = castlingFlags state

setCastlingFlags :: GameState -> String -> GameState
setCastlingFlags state flags = state { castlingFlags = flags }

getLastDoubleStep :: GameState -> Maybe (Int, Int)
getLastDoubleStep state = lastDoubleStep state

setLastDoubleStep :: GameState -> Maybe (Int, Int) -> GameState
setLastDoubleStep state move = state { lastDoubleStep = move }

isChecked :: Player -> GameState -> Bool
isChecked player state = case getCellOfPiece (Piece player King) (gameField state) of
                              Nothing -> False --King doesn't exist, which should never happen, as game ends once king is in checkmate!
                              Just kingCell -> if isAttacked kingCell (getOpponentOf player) state then True
                                                                                                   else False

--checks if a piece on given cell is reachable/attacked by given player
isAttacked :: Cell -> Player -> GameState -> Bool
isAttacked cell player state = any (isAttackedByMove cell player state) [possibleMoves | from <- getCellsOfPlayer player (gameField state), possibleMoves <- getPossibleMovesForPiece from state]

isAttackedByMove :: Cell -> Player -> GameState -> Move -> Bool
isAttackedByMove cell player state (Castle _) = False
isAttackedByMove cell player state (PawnPromotion _ to _) = if cell == to then True
                                                                          else False
isAttackedByMove cell player state (Move _ to) = if cell == to then True
                                                               else False

canMove :: Player -> GameState -> Bool
canMove player state = if getLegalMovesForPlayer player state == [] then False
                                                                    else True

isCheckmate :: Player -> GameState -> Bool
isCheckmate player state = isChecked player state && canMove player state

isStalemate :: Player -> GameState -> Bool
isStalemate player state = not $ isChecked player state && canMove player state

--gets the type of move for a move from a cell to another cell 
getTypeOfMove :: Cell -> Cell -> GameState -> MoveType 
getTypeOfMove from to state = "TODO"

getLegalMovesForPlayer :: Player -> GameState -> [Move]
getLegalMovesForPlayer player state = [possibleMove | from <- getCellsOfPlayer player (gameField state), possibleMove <- getPossibleMovesForPiece from state]

--TODO: move function to Chess
getPossibleMovesForPiece :: Cell -> GameState -> [Move]
getPossibleMovesForPiece from state | currentPhase state /= Running = [] --when game is not running, there shouldn't be moves possible 
                                    | otherwise =
                                        case get from (gameField state) of
                                             Just (Piece player King) -> []
                                             Just (Piece player _) -> if isChecked player state then [] --if checked only king is moveable
                                                                                                else []
                                             _ -> [] --when given cell is not of player, there shouldn't be moves possible, maybe give error as this should never be called in program
    where player = currentPlayer state
          
--NC means no check, it doesnt check if the move would put self in check or player is currently in check 
getPossibleMovesForPieceNC :: Cell -> Player -> GameState -> [Move]
getPossibleMovesForPieceNC from player state = 
    case get from (gameField state) of
         Just (Piece player Pawn) -> []
         Just (Piece player Knight) -> map (\to -> Move from to) (getReachableKnight from player setInitialPieces)
         Just (Piece player Bishop) -> map (\to -> Move from to) (getReachableBishop from player setInitialPieces)
         Just (Piece player Rook) -> map (\to -> Move from to) (getReachableRook from player setInitialPieces)
         Just (Piece player Queen) -> map (\to -> Move from to) (getReachableQueen from player setInitialPieces)
         Just (Piece player King) ->  map (\to -> Move from to) (getReachableKing from player setInitialPieces)
         _ -> []

--gets all cells that are reachable (either free or opponent piece) from given cell in given direction
getReachablePath :: Cell -> Player -> GameField -> Int -> Int -> [Cell]
getReachablePath from player field x y | not $ isWithinBounds newCell = []
                                       | otherwise = case get newCell field of
                                                          Nothing -> [newCell] ++ getReachablePath newCell player field x y
                                                          Just piece -> if getPlayer piece == player then []
                                                                                                     else [newCell]
    where newCell = (getRow from + x, getColumn from + y)

isFree :: Cell -> GameField -> Bool
isFree cell field = case get cell field of
                         Nothing -> True
                         _ -> False

--gets all cells that are free in given list of cells   
getFree :: [Cell] -> GameField -> [Cell]
getFree [] _ = []
getFree (h:t) field | isFree h field = [h] ++ getFree t field 
                    | otherwise = getFree t field

isOpponent :: Cell -> Player -> GameField -> Bool
isOpponent cell player field = case get cell field of
                                    Just piece -> if getPlayer piece == getOpponentOf player then True
                                                                                             else False
                                    Nothing -> False
  
--gets all cells that are reachable in given list of cells 
getOpponent :: [Cell] -> Player -> GameField -> [Cell]
getOpponent [] _ _ = []
getOpponent (h:t) player field | isOpponent h player field = [h] ++ getOpponent t player field
                               | otherwise = getOpponent t player field

--gives all cells that would be reachable with bishop from given cell
getReachableBishop :: Cell -> Player -> GameField -> [Cell]
getReachableBishop from player field =  reachInDir 1 1 ++ reachInDir (-1) 1 ++ reachInDir (-1) (-1) ++ reachInDir 1 (-1)
    where reachInDir = getReachablePath from player field
 
--gives all cells that would be reachable with rook from given cell
getReachableRook :: Cell -> Player -> GameField -> [Cell]
getReachableRook from player field =  reachInDir 1 0 ++ reachInDir (-1) 0 ++ reachInDir 0 1 ++ reachInDir 0 (-1)
    where reachInDir = getReachablePath from player field
          
getReachableQueen :: Cell -> Player -> GameField -> [Cell]
getReachableQueen from player field = getReachableBishop from player field ++ getReachableQueen from player field

getReachablePawn :: Cell -> Player -> GameField -> [Cell]
getReachablePawn from player field = []

getReachableKnight :: Cell -> Player -> GameField -> [Cell]
getReachableKnight from player field = getFree possible field ++ getOpponent possible player field
    where possible = [cell | (x,y) <- list, cell <- [(getRow from + x,getColumn from + y)], isWithinBounds cell]
          list = [(-2,-1),(-2,1),(2,-1),(2,1),(-1,-2),(-1,2),(1,-2),(1,2)]

getReachableKing :: Cell -> Player -> GameField -> [Cell]
getReachableKing from player field = getFree possible field ++ getOpponent possible player field
    where possible = [cell | (x,y) <- list, cell <- [(getRow from + x,getColumn from + y)], isWithinBounds cell]
          list = [(-1,-1),(-1,0),(-1,1),(0,1),(0,-1),(1,-1),(1,0),(1,1)]
