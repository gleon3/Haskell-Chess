module Model.Ai.StateAssessor where

import Model.GameState
import Model.Ai.PieceAmountAssessor as PieceAmountAssessor

computeValue :: GameState -> Float
computeValue state = PieceAmountAssessor.computeValue state
