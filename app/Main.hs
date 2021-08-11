{-# LANGUAGE RecordWildCards, FlexibleContexts #-}

module Main where

import System.Environment

import CLI
import Core
import Colors

main :: IO ()
main = getArgs >>= act

act :: [String] -> IO ()
act ["help"]   = helpT root
act ["--help"] = helpT root
act ["-h"]     = helpT root
act []         = helpT root
act args = do
  e <- exists root args
  if e then run root args
    else putStrLn$ "Couldn't find your command." #Error
