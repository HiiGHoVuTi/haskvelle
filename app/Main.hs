{-# LANGUAGE RecordWildCards, FlexibleContexts #-}

module Main where

import System.Environment

import CLI
import Core
import Colors

-- | Simple main
main :: IO ()
main = getArgs >>= act

-- | Detects any of the help arguments, if none, execute the root Atom
act :: [String] -> IO ()
act ["help"]   = helpT root
act ["--help"] = helpT root
act ["-h"]     = helpT root
act []         = helpT root
act args = do
  e <- exists root args
  if e then run root args
    else putStrLn$ "Couldn't find your command." #Error
