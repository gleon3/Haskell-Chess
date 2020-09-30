module Model.Move where

import Model.Cell
import Model.Piece
import Model.Player

data Move = Move Cell Cell
          | DoubleStepMove Cell Cell
          | EnPassant Cell Cell Cell --from to lastDoubleStep
          | PawnPromotion Cell Cell PieceType
          | Castle { kingside :: Bool, player :: Player }
          deriving (Eq,Show,Read)  
    
toNotation :: Move -> String
toNotation (Move from to) = formatCell from ++ formatCell to
toNotation (DoubleStepMove from to) = formatCell from ++ formatCell to
toNotation (EnPassant from to _) = formatCell from ++ formatCell to ++ "e"
toNotation (PawnPromotion from to pieceType) = formatCell from ++ formatCell to ++ show pieceType
toNotation (Castle True _) = "O-O"
toNotation (Castle False _) = "O-O-O"  

getSourceCell :: Move -> Cell
getSourceCell (Move from _) = from
getSourceCell (PawnPromotion from _ _) = from
getSourceCell (EnPassant from _ _) = from
getSourceCell (DoubleStepMove from _) = from
getSourceCell (Castle _ White) = (0,4)
getSourceCell (Castle _ Black) = (7,4)

getTargetCell :: Move -> Cell
getTargetCell (Move _ to) = to
getTargetCell (PawnPromotion _ to _) = to 
getTargetCell (EnPassant _ to _) = to
getTargetCell (DoubleStepMove _ to) = to
getTargetCell (Castle True White) = (0,6)
getTargetCell (Castle True Black) = (7,6)
getTargetCell (Castle False White) = (0,2)
getTargetCell (Castle False Black) = (7,2)
