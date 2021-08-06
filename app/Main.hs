{-# LANGUAGE RecordWildCards, FlexibleContexts #-}

module Main where

import Control.Monad                 (void)
import Data.Default                  (def)
import System.Console.StructuredCLI

import Core
import Events
import Colors

main :: IO ()
main = do
    eventsThread
    void $ runCLI ("Velle" #Name) def root
