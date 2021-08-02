
module Core (
  root, velleInitWork
            ) where

import System.Console.StructuredCLI
import System.IO()
import System.Directory
import System.Process
import System.Exit
import Colors

import Control.Monad (forM_)

import Data.Configurator
import Data.Configurator.Types
import Data.HashMap.Strict hiding (map)
import Data.List
import Data.Text (unpack, pack)

import Config
import Install

root :: Commands ()
root = do
  core
  config$ do
    core
    velleInit
  install core

core :: Commands ()
core = do
  exit'
  up
  shell'
  run

run :: Commands ()
run = colorCustom "run" "runs a command from config" $ \x -> do
  files <- listDirectory ".velle"
  cfg <- load . map Optional . map (".velle/" <>) $ files
  map' <- let
    keyPrefix    = "commands"
    map''        = getMap cfg
    in filterWithKey (\k _ -> isPrefixOf keyPrefix . unpack$ k) <$> map''
  let
    key    = "commands."<>head x
    values = map' !? pack key
    list   = values >>= convert :: Maybe [String]
  _ <- case list of
    Just cmds -> do
      forM_ cmds callCommand
    Nothing   -> putStrLn ("No commands found." #Error)
  return NoAction


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
  putStrLn ("Creating .velle..." #OK)
  -- appendFile ".gitignore" "\n.velle/"
  putStrLn ("Project name: " <> cwd' #Name)
  appendFile ".velle/main.cfg" ("project {\n  name = \""<>cwd'<>"\"\n}\ncommands {\n\n}")
  putStrLn ("Velle init !" #Success)

up :: Commands ()
up = colorCommand "up" "goes up one level" $ return $ LevelUp 1

exit' :: Commands ()
exit' = colorCommand "exit" "exits the shell" $ do
  putStrLn ("See you soon !"#OK)
  _ <- exitWith ExitSuccess
  return NoAction
