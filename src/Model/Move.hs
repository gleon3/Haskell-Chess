module Model.Move ( Move(Move,PawnPromotion,Castle), MoveType, getMoveType ) where

import Model.Cell
import Model.Piece

data Move = Move Cell Cell
          | PawnPromotion Cell Cell PieceType
          | Castle { kingSide :: Bool }
          deriving (Eq, Show)
  
--MoveType is either Move, PawnPromotion or Castle  
type MoveType = String

getMoveType :: Move -> MoveType
getMoveType (Move _ _) = "Move"
getMoveType (PawnPromotion _ _ _) = "PawnPromotion"
getMoveType (Castle _) = "Castle"

getSourceCell :: Move -> Cell
getSourceCell (Move from _) = from
getSourceCell (PawnPromotion from _ _) = from

getTargetCell :: Move -> Cell
getTargetCell (Move _ to) = to
getTargetCell (PawnPromotion _ to _) = to 
