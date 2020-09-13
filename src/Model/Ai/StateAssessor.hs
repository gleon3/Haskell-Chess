module Model.Ai.StateAssessor where

import Model.Ai.PieceAmountAssessor as PieceAmountAssessor
import Model.Ai.WinAssessor as WinAssessor
import Model.GameState

computeValue :: GameState -> Int
computeValue state = PieceAmountAssessor.computeValue state + WinAssessor.computeValue state
