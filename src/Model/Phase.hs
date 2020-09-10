module Model.Phase where

data Phase = Running
           | Finished
           | Waiting
           deriving (Eq,Show)
