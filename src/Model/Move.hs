module Model.Move where

import Model.Cell
import Model.Piece

data Move = Move Cell Cell
          | DoubleStepMove Cell Cell
          | EnPassant Cell Cell Cell --from to lastDoubleStep
          | PawnPromotion Cell Cell PieceType
          | Castle { kingside :: Bool }
          deriving (Eq,Show,Read)
          
--instance Show Move where
 --   show (Move from to) = formatCell from ++ formatCell to
 --   show (DoubleStepMove from to) = formatCell from ++ formatCell to
 --   show (EnPassant from to _) = formatCell from ++ formatCell to ++ "e"
  --  show (PawnPromotion from to pieceType) = formatCell from ++ formatCell to ++ show pieceType
--    show (Castle True) = "O-O"
--    show (Castle False) = "O-O-O"    
    
toNotation :: Move -> String
toNotation (Move from to) = formatCell from ++ formatCell to
toNotation (DoubleStepMove from to) = formatCell from ++ formatCell to
toNotation (EnPassant from to _) = formatCell from ++ formatCell to ++ "e"
toNotation (PawnPromotion from to pieceType) = formatCell from ++ formatCell to ++ show pieceType
toNotation (Castle True) = "O-O"
toNotation (Castle False) = "O-O-O"   

getSourceCell :: Move -> Cell
getSourceCell (Move from _) = from
getSourceCell (PawnPromotion from _ _) = from
getSourceCell (EnPassant from _ _) = from
getSourceCell (DoubleStepMove from _) = from
getSourceCell (Castle _) = (0,4)

getTargetCell :: Move -> Cell
getTargetCell (Move _ to) = to
getTargetCell (PawnPromotion _ to _) = to 
getTargetCell (EnPassant _ to _) = to
getTargetCell (DoubleStepMove _ to) = to
getTargetCell (Castle True) = (0,6)
getTargetCell (Castle False) = (0,2)
