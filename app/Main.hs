{-# LANGUAGE OverloadedStrings #-}
module Main where

import GrabCite.Server

import Control.Logger.Simple
import System.Environment

main :: IO ()
main =
    withGlobalLogging (LogConfig Nothing True) $
    do args <- getArgs
       case args of
         (port:_) -> runServer (read port)
         _ -> logError "Usage: ./grabcite-server [port]"
