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
        show (GameState _ gameField Finished (Just winner) _ _ ) = "Thanks for playing!\n" ++ "Player " ++ show winner ++ " has won!\n" ++ toString gameField
        show (GameState _ gameField Finished Nothing _ _ ) = "Thanks for playing! \nIt's a draw!\n" ++ toString gameField  
        show (GameState currentPlayer gameField _ _ _ castlingFlags) = "player to move: " ++ show currentPlayer ++ " " ++ castlingFlags ++ "\n" ++ toString gameField
        
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

removeCastlingFlag :: Char -> GameState -> GameState
removeCastlingFlag flag state = setCastlingFlags state [x | x <- (getCastlingFlags state), x /= flag]

removeCastle :: Bool -> Player -> GameState -> GameState
removeCastle kingside player state | player == White = if kingside then removeCastlingFlag 'K' state
                                                                   else removeCastlingFlag 'Q' state
                                   | otherwise = if kingside then removeCastlingFlag 'k' state
                                                             else removeCastlingFlag 'q' state

getLastDoubleStep :: GameState -> Maybe (Int, Int)
getLastDoubleStep state = lastDoubleStep state

setLastDoubleStep :: GameState -> Maybe (Int, Int) -> GameState
setLastDoubleStep state move = state { lastDoubleStep = move }

--checks if a piece on given cell is reachable/attacked by given player
isAttacked :: Cell -> Player -> GameState -> Bool
isAttacked cell player state = any (isAttackedByMove cell player state) [possibleMoves | from <- getCellsOfPlayer player (gameField state), possibleMoves <- getPossibleMovesForPieceNC from player state]

isAttackedByMove :: Cell -> Player -> GameState -> Move -> Bool
isAttackedByMove cell player state (Castle _) = False
isAttackedByMove cell player state (PawnPromotion _ to _) = cell == to
isAttackedByMove cell player state (Move _ to) = cell == to
isAttackedByMove cell player state (EnPassant _ _ _) = False
isAttackedByMove cell player state (DoubleStepMove _ _) = False

isChecked :: Player -> GameState -> Bool
isChecked player state = case getCellOfPiece (Piece player King) (gameField state) of
                              Nothing -> False --King doesn't exist, which should never happen, as game ends once king is in checkmate!
                              Just kingCell -> if isAttacked kingCell (getOpponentOf player) state then True
                                                                                                   else False

--gets the type of move for a move from a cell to another cell, even for absurd moves, which would be sorted out in move function! 
getTypeOfMove :: Cell -> Cell -> GameState -> Move
getTypeOfMove from to state | fromIsPawn && getRow to == getBaseRowIndex (getOpponentOf (currentPlayer state)) = PawnPromotion from to Queen--Queen is a placeholder as player still has to choose promotionpiece
                            | fromIsPawn = case lastDoubleStep state of
                                                Just cell -> if getRow to == getRow cell - getDirection (currentPlayer state) && getColumn to == getColumn cell then EnPassant from to cell
                                                                                                                                                                else Move from to
                                                _ -> Move from to 
                            | fromIsPawn && abs (getRow from - getRow to) == 2 = DoubleStepMove from to
                            | fromIsKing && from == castleSource && to == castleTargetKingside = Castle True
                            | fromIsKing && from == castleSource && to == castleTargetQueenside = Castle False
                            | otherwise = Move from to
    where castleSource = (getBaseRowIndex (currentPlayer state), 4)
          castleTargetKingside = (getBaseRowIndex (currentPlayer state), 6)
          castleTargetQueenside = (getBaseRowIndex (currentPlayer state), 2)
          fromIsPawn = get from (gameField state) == Just (Piece (currentPlayer state) Pawn)
          fromIsKing = get from (gameField state) == Just (Piece (currentPlayer state) King)

--NC means no check, it doesnt check if the move would put self in check or player is currently in check (so castling is not included!) 
getPossibleMovesForPieceNC :: Cell -> Player -> GameState -> [Move]
getPossibleMovesForPieceNC from player state = 
    case get from (gameField state) of
         Just (Piece player Pawn) -> map (\to -> Move from to) (getReachablePawn from player setInitialPieces) ++ getSpecialMovesPawn from player state
         Just (Piece player Knight) -> map (\to -> Move from to) (getReachableKnight from player setInitialPieces)
         Just (Piece player Bishop) -> map (\to -> Move from to) (getReachableBishop from player setInitialPieces)
         Just (Piece player Rook) -> map (\to -> Move from to) (getReachableRook from player setInitialPieces)
         Just (Piece player Queen) -> map (\to -> Move from to) (getReachableQueen from player setInitialPieces)
         Just (Piece player King) ->  map (\to -> Move from to) (getReachableKing from player setInitialPieces)
         _ -> []

--gets all cells that are reachable (either free or opponent piece) from given cell in given direction
getReachablePath :: Cell -> Player -> GameField -> Int -> Int -> [Cell]
getReachablePath from player field row col | not $ isWithinBounds newCell = []
                                           | otherwise = case get newCell field of
                                                          Nothing -> [newCell] ++ getReachablePath newCell player field row col
                                                          Just piece -> if getPlayer piece == player then []
                                                                                                     else [newCell]
    where newCell = (getRow from + row, getColumn from + col)

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

--gets all cells that would be reachable with bishop from given cell
getReachableBishop :: Cell -> Player -> GameField -> [Cell]
getReachableBishop from player field =  reachInDir 1 1 ++ reachInDir (-1) 1 ++ reachInDir (-1) (-1) ++ reachInDir 1 (-1)
    where reachInDir = getReachablePath from player field
          
--gets all cells that would be reachable with knight from given cell
getReachableKnight :: Cell -> Player -> GameField -> [Cell]
getReachableKnight from player field = getFree possible field ++ getOpponent possible player field
    where possible = [cell | (row,col) <- list, cell <- [(getRow from + row,getColumn from + col)], isWithinBounds cell]
          list = [(-2,-1),(-2,1),(2,-1),(2,1),(-1,-2),(-1,2),(1,-2),(1,2)]
 
--gets all cells that would be reachable with rook from given cell
getReachableRook :: Cell -> Player -> GameField -> [Cell]
getReachableRook from player field =  reachInDir 1 0 ++ reachInDir (-1) 0 ++ reachInDir 0 1 ++ reachInDir 0 (-1)
    where reachInDir = getReachablePath from player field
   
--gets all cells that would be reachable with queen from given cell
getReachableQueen :: Cell -> Player -> GameField -> [Cell]
getReachableQueen from player field = getReachableBishop from player field ++ getReachableRook from player field

--gets all cells that would be reachable with king from given cell - castling is not included
getReachableKing :: Cell -> Player -> GameField -> [Cell]
getReachableKing from player field = getFree possible field ++ getOpponent possible player field
    where possible = [cell | (row,col) <- list, cell <- [(getRow from + row,getColumn from + col)], isWithinBounds cell]
          list = [(-1,-1),(-1,0),(-1,1),(0,1),(0,-1),(1,-1),(1,0),(1,1)]
          
castlingAvailable :: Player -> Bool -> GameState -> Bool
castlingAvailable White kingside state = if kingside then elem 'K' (getCastlingFlags state)
                                                     else elem 'Q' (getCastlingFlags state)
castlingAvailable Black kingside state = if kingside then elem 'k' (getCastlingFlags state)
                                                     else elem 'q' (getCastlingFlags state)
                                                     
--does not check if king would castle into check!
getCastlingMoves :: Player -> GameState -> [Move]
getCastlingMoves player state | isChecked player state = []
                              | canCastleKingside && canCastleQueenside = [Castle True, Castle False]
                              | canCastleKingside = [Castle True]
                              | canCastleQueenside = [Castle False]
                              | otherwise = []
    where canCastleKingside = castlingAvailableKingside && canMoveThroughKingside && not pathAttackedKingside
          canCastleQueenside = castlingAvailableQueenside && canMoveThroughQueenside && not pathAttackedQueenside
          castlingAvailableKingside = castlingAvailable player True state
          castlingAvailableQueenside = castlingAvailable player False state
          canMoveThroughKingside = isFree (getBaseRowIndex player, 5) (gameField state) && isFree (getBaseRowIndex player, 6) (gameField state)
          canMoveThroughQueenside = isFree (getBaseRowIndex player, 3) (gameField state) && isFree (getBaseRowIndex player, 2) (gameField state) && isFree (getBaseRowIndex player, 1) (gameField state)
          pathAttackedKingside = isAttacked (getBaseRowIndex player, 3) (getOpponentOf player) state
          pathAttackedQueenside = isAttacked (getBaseRowIndex player, 5) (getOpponentOf player) state

--standard pawn moves, capture diagonally, move horizontally, doesn't include pawn promotion
getReachablePawn :: Cell -> Player -> GameField -> [Cell]
getReachablePawn from player field = getFree cellsMove field ++ getOpponent cellsCapture player field
    where cellsMove = [cell | cell <- [(getRow from + getDirection player, getColumn from)], isWithinBounds cell, getRow cell /= getBaseRowIndex (getOpponentOf player)]
          cellsCapture = [cell | cell <- [(getRow from + getDirection player, getColumn from + x) | x <- [-1,1]], isWithinBounds cell, getRow cell /= getBaseRowIndex (getOpponentOf player)]
         
--special pawn moves include en passant, pawn promotion and double pawn move         
getSpecialMovesPawn :: Cell -> Player -> GameState -> [Move]
getSpecialMovesPawn from player state = getEnPassantMoves ++ getDoublePawnMove ++ getPawnPromotion
    where getDoublePawnMove = [DoubleStepMove from (getRow from + 2*getDirection player, getColumn from) | getRow from == getBaseRowIndex player + getDirection player]
          getEnPassantMoves = case lastDoubleStep state of
                                   Nothing -> [] 
                                   Just cell -> [EnPassant from (getRow cell + getDirection player, getColumn cell) cell | getRow from == getRow cell, abs(getColumn from - getColumn cell) == 1]
          getPawnPromotion = [PawnPromotion from to pieceType | to <- getReachablePromoting, pieceType <- [Queen, Knight, Rook, Bishop]]
          getReachablePromoting = getFree cellsMove (gameField state) ++ getOpponent cellsCapture player (gameField state)
          cellsMove = [cell | cell <- [(getRow from + getDirection player, getColumn from)], isWithinBounds cell, getRow cell == getBaseRowIndex (getOpponentOf player)]
          cellsCapture = [cell | cell <- [(getRow from + getDirection player, getColumn from + x) | x <- [-1,1]], isWithinBounds cell, getRow cell == getBaseRowIndex (getOpponentOf player)]
