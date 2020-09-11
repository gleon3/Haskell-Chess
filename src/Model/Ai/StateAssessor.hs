module Model.Ai.StateAssessor where

import Model.GameState
import Model.Ai.PieceAmountAssessor as PieceAmountAssessor
import Model.Ai.WinAssessor as WinAssessor

computeValue :: GameState -> Int
computeValue state = PieceAmountAssessor.computeValue state + WinAssessor.computeValue state
