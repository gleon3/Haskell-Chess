module Main where

import Model.Network.Server

port = "5000"

main :: IO ()
main = startServer port
