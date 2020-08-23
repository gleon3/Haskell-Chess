module Model.Player ( Player(White, Black), getOpponentOf ) where

data Player = Black
            | White
            deriving (Eq, Show)
            
getOpponentOf :: Player -> Player
getOpponentOf Black = White
getOpponentOf White = Black
