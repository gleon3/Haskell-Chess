module Model.AiChess where

import Model.Chess
import Model.GameState
import Model.Move

import Model.Ai.StateAssessor
                                         
getAiMove :: Int -> Bool -> GameState -> Maybe Move
getAiMove depth maximizingPlayer state = let possibleMoves = getLegalMovesForPlayer (currentPlayer state) state
                                             possibleStates = map (\move -> performMove move state) possibleMoves
                                         in if possibleMoves == [] then Nothing
                                                                   else let maximumIndex = snd . maximum $ zip (map (miniMax (depth-1) (not maximizingPlayer)) possibleStates) [0..]
                                                                            minimumIndex = snd . minimum $ zip (map (miniMax (depth-1) (not maximizingPlayer)) possibleStates) [0..]
                                                                        in if maximizingPlayer then Just (possibleMoves !! maximumIndex)
                                                                                               else Just (possibleMoves !! minimumIndex)

miniMax :: Int -> Bool -> GameState -> Int
miniMax depth maximizingPlayer state = let children = [performMove x state | x <- getLegalMovesForPlayer (currentPlayer state) state] 
                                       in if depth == 0 || isStalemate (currentPlayer state) state || isCheckmate (currentPlayer state) state
                                              then computeValue state
                                              else if maximizingPlayer then maximum (map (miniMax (depth-1) False) children)
                                                                       else minimum (map (miniMax (depth-1) True) children)
