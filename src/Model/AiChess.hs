module Model.AiChess where

import Model.Move
import Model.GameState
import Model.Chess

import Model.Ai.StateAssessor

--very high number!
inf = read "Infinity" :: Float

getAiMove :: Int -> Bool -> GameState -> Maybe Move
getAiMove depth maximizingPlayer state = let possibleMoves = getLegalMovesForPlayer (currentPlayer state) state
                                         in miniMaxMove possibleMoves depth maximizingPlayer state (-inf) Nothing
    where miniMaxMove [] depth maximizingPlayer state bestValue bestMove = bestMove
          miniMaxMove possibleMoves depth maximizingPlayer state bestValue bestMove = let currentMove = (head possibleMoves)
                                                                                          value = miniMax (depth-1) (not maximizingPlayer) (executeMove currentMove state)
                                                                                      in if value >= bestValue then miniMaxMove (tail possibleMoves) depth maximizingPlayer state value (Just currentMove)
                                                                                                               else miniMaxMove (tail possibleMoves) depth maximizingPlayer state bestValue bestMove

miniMax :: Int -> Bool -> GameState -> Float
miniMax depth maximizingPlayer state = let children = [performMove x state | x <- getLegalMovesForPlayer (currentPlayer state) state] 
                                       in if depth == 0 || isStalemate (currentPlayer state) state || isCheckmate (currentPlayer state) state
                                              then computeValue state
                                              else if maximizingPlayer then maximum (map (miniMax (depth-1) False) children)
                                                                       else minimum (map (miniMax (depth-1) True) children)
