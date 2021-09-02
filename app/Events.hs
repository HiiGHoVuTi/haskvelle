
{-# LANGUAGE NumericUnderscores #-}

module Events (
  events
              ) where

import Control.Concurrent (threadDelay)
import Control.Monad
import System.Directory
import Data.Time.Clock

import Interpreter
import Utils
import CLI
import Colors

events :: Atom
events = colorCommand "watch" "watches for events in the project" $ return eventsThread

eventsThread :: IO ()
eventsThread = forever $ do
  -- dir <- getAppUserDataDirectory "velle"
  path    <- getConfigPropFromFolder ".velle" "commands.on.changed.folder"
  command <- getConfigPropFromFolder ".velle" "commands.on.changed.do"
  _ <- (traverseForChange
        <$> command
        <*> path)
       ?: return()
  threadDelay 1_000_000 -- wait a second


traverseForChange :: String -> String -> IO ()
traverseForChange cmd path = do
  dirs <- listDirectory path
  now  <- getCurrentTime
  void$
    forM dirs (branch now)
  where
    branch :: UTCTime -> FilePath -> IO ()
    branch now p = do
      isDir <- doesDirectoryExist (path</>p)
      if isDir
        then traverseForChange cmd (path</>p)
        else voidIOSafe$ fileCheck cmd now (path</>p)

fileCheck :: String -> UTCTime -> String -> IO ()
fileCheck cmd now file = do
  modif   <- getModificationTime file
  let diff = diffUTCTime now modif
  if diff < (1 :: NominalDiffTime)
    then (eval imports ".velle/") cmd
    else return()
  where
    imports :: [(String, Int -> IO String)]
    imports = [("changedFile", \_ -> return file)]
