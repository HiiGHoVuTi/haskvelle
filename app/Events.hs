
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

-- | The command node that creates and runs the event thread
events :: Atom
events = colorCommand "watch" "watches for events in the project" $ return eventsThread

{- | Every second, looks for "commands.on.changed", and recursively checks through folders of the project to find changes and execute the action
  The format of "commands.on.changed" is:
```
commands {
  on {
    changed =
      [ ["folder"       , "action"]
      , ["folder/folder", "action"]
      ]
  }
}
```
-}
eventsThread :: IO ()
eventsThread = forever $ do
  path'cmd <- getConfigPropFromFolder ".velle" "commands.on.changed" -- Reads the Maybe Value
    |> fmap (?: [])                                                  -- Replaces with an empty array if Nothing
  forM path'cmd traverseForChange'
  >> threadDelay 1_000_000 -- wait a second

  where
    traverseForChange' [path, command] = traverseForChange command path
    traverseForChange' _               = putStrLn ("Invalid configuration file." #Error)


-- | Recursively traverses the path, and executes "cmd" if a file has changed recently.
traverseForChange :: String -> String -> IO ()
traverseForChange cmd path = do
  dirs <- listDirectory path
  now  <- getCurrentTime
  forM_ dirs (branch now)
  where
    branch :: UTCTime -> FilePath -> IO ()
    branch now p = do
      isDir <- doesDirectoryExist (path</>p)
      if isDir
        then traverseForChange cmd (path</>p)
        else voidIOSafe$ fileCheck cmd now (path</>p)

-- | Performs the file-level work, by checking the modification time, and executing the command if it's less than a second ago.
fileCheck :: String -> UTCTime -> String -> IO ()
fileCheck cmd now file = do
  modif   <- getModificationTime file
  let diff = diffUTCTime now modif
  when (diff < (1 :: NominalDiffTime)) $
    eval imports ".velle/" cmd
  where
    imports :: [(String, Int -> IO String)]
    imports = [("changedFile", \_ -> pure file)]
