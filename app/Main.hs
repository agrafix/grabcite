module Main where

import GrabCite.Server

import System.Environment

main :: IO ()
main =
    do args <- getArgs
       case args of
         (port:_) -> runServer (read port)
         _ -> putStrLn "Usage: ./grabcite-server [port]"
