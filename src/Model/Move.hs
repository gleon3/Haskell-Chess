module Model.Move where

import Model.Cell
import Model.Piece

data Move = Move Cell Cell
          | DoubleStepMove Cell Cell
          | EnPassant Cell Cell
          | PawnPromotion Cell Cell PieceType
          | Castle { kingSide :: Bool }
          deriving (Eq, Show)
  
--MoveType is either Move, PawnPromotion or Castle  
type MoveType = String

getMoveType :: Move -> MoveType
getMoveType (Move _ _) = "Move"
getMoveType (PawnPromotion _ _ _) = "PawnPromotion"
getMoveType (Castle _) = "Castle"
getMoveType (EnPassant _ _) = "EnPassant"
getMoveType (DoubleStepMove _ _) = "DoubleStepMove"

getSourceCell :: Move -> Cell
getSourceCell (Move from _) = from
getSourceCell (PawnPromotion from _ _) = from
getSourceCell (EnPassant from _) = from
getSourceCell (DoubleStepMove from _) = from
getSourceCell (Castle _) = (0,4)

getTargetCell :: Move -> Cell
getTargetCell (Move _ to) = to
getTargetCell (PawnPromotion _ to _) = to 
getTargetCell (EnPassant _ to) = to
getTargetCell (DoubleStepMove _ to) = to
getTargetCell (Castle True) = (0,6)
getTargetCell (Castle False) = (0,2)
