
module Core (
  root, velleInitWork
            ) where

import System.Console.StructuredCLI
import System.IO()
import System.Directory
import System.Process
import System.Exit
import Colors

import Config

root :: Commands ()
root = do
  core
  config$ do
    core
    velleInit

core :: Commands ()
core = do
  exit'
  up
  shell'

shell' :: Commands ()
shell' = colorCommand "shell" "opens a normal shell" $ do
  _ <- callCommand "zsh"
  return NoAction

velleInit :: Commands ()
velleInit = colorCommand "init" "initialize velle in cwd" $ do
  _ <- velleInitWork
  return NoAction

velleInitWork :: IO ()
velleInitWork = do
  cwd' <- (reverse.takeWhile (/= '/').reverse) <$> getCurrentDirectory
  createDirectory ".velle"
  putStrLn ("Creating .velle and .gitignore..." #OK)
  appendFile ".gitignore" "\n.velle/"
  putStrLn ("Project name: " <> cwd' #Name)
  appendFile ".velle/main.cfg" ("project {\n  name = \""<>cwd'<>"\"\n}")
  putStrLn ("Velle init !" #OK)

up :: Commands ()
up = colorCommand "up" "goes up one level" $ return $ LevelUp 1

exit' :: Commands ()
exit' = colorCommand "exit" "exits the shell" $ do
  putStrLn ("See you soon !"#OK)
  _ <- exitWith ExitSuccess
  return NoAction
