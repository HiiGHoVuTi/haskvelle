
module Core (
  root, velleInitWork, eval
            ) where

-- import System.Console.StructuredCLI
import CLI hiding (run)
import System.Directory
import Colors
import Utils

import Control.Monad

import Config
import Install
import Repo
import Interpreter
import Events

root :: [Atom]
root =
  [ run
  , events
  , load
  , local
  , config >+ [velleInit]
  , install
  ]


run :: Atom
run = colorCommand "run" "runs a command from config" $ \x -> do
  list <- getConfigPropFromFolder ".velle" ("commands."<>head x) :: IO (Maybe [String])
  _ <- case list of
    Just cmds -> forM_ cmds $ voidIOSafe . (eval noImports ".velle/")
    Nothing   -> putStrLn ("No commands found." #Error)
  return ()
  where
    noImports = [] :: [(String, () -> IO ())]


velleInit :: Atom
velleInit = colorCommand "init" "initialize velle in cwd" $ \_ -> do
  _ <- velleInitWork
  return ()

velleInitWork :: IO ()
velleInitWork = do
  cwd' <- (reverse.takeWhile (/= '/').reverse) <$> getCurrentDirectory
  createDirectory ".velle"
  putStrLn ("Creating .velle..." #OK)
  -- appendFile ".gitignore" "\n.velle/"
  putStrLn ("Project name: " <> cwd' #Name)
  appendFile ".velle/main.cfg" ("project {\n  name = \""<>cwd'<>"\"\n}\ncommands {\n\n}")
  putStrLn ("Velle init !" #Success)
