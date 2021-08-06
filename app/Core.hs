
module Core (
  root, velleInitWork, eval
            ) where

import System.Console.StructuredCLI
import System.Directory
import System.Process
import System.Exit
import Colors
import Utils

import Control.Monad

import Config
import Install
import Repo
import Interpreter

root :: Commands ()
root = do
  core
  config$ do
    coreDown
    velleInit
  install coreDown
  local coreDown
  load

coreDown :: Commands ()
coreDown = do
  core
  up

core :: Commands ()
core = do
  exit'
  shell'
  run

run :: Commands ()
run = colorCustom "run" "runs a command from config" $ \x -> do
  list <- getConfigPropFromFolder ".velle" ("commands."<>head x) :: IO (Maybe [String])
  _ <- case list of
    Just cmds -> forM_ cmds $ voidIOSafe . (eval noImports ".velle/")
    Nothing   -> putStrLn ("No commands found." #Error)
  return NoAction
  where
    noImports = [] :: [(String, () -> IO ())]


shell' :: Commands ()
shell' = colorCommand "shell" "opens a normal shell" $ do
  dir <- getAppUserDataDirectory "velle"
  cmd <- getConfigPropFromFolder dir "user-preferences.shell"
  case cmd of
         Just c  -> voidIOSafe . callCommand$ c
         Nothing -> putStrLn ("No default shell provided." #Error)
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
exit' = colorCommand "exit" "exits the velle shell" $ do
  putStrLn ("See you soon !"#OK)
  _ <- exitWith ExitSuccess
  return NoAction
