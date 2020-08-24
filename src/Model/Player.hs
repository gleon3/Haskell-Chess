module Model.Player where

data Player = Black
            | White
            deriving (Eq, Show)
            
getOpponentOf :: Player -> Player
getOpponentOf Black = White
getOpponentOf White = Black
