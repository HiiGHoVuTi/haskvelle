
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

-- | The main set of commands, including everything the user sees when typing "velle -h"
root :: [Atom]
root =
  [ run
  , events
  , load
  , local
  , config velleInit
  , install
  ]

-- | Interprets a command from the commands section in the config. Commands may be shell commands, or "interp file.js", where the file is written in velle-js
run :: Atom
run = colorCommand "run" "runs a command from config" $ \x -> do
  list <- getConfigPropFromFolder ".velle" ("commands."<>head x) :: IO (Maybe [String])
  _ <- case list of
    Just cmds -> forM_ cmds $ voidIOSafe . (eval noImports ".velle/")
    Nothing   -> putStrLn ("No commands found." #Error)
  return ()
  where
    noImports = [] :: [(String, () -> IO ())]

-- | Inits the .velle folder in the cwd
velleInit :: Atom
velleInit = colorCommand "init" "initialize velle in cwd" $ const velleInitWork

-- | The IO logic and logging behind setting up the .velle folder
velleInitWork :: IO ()
velleInitWork = do
  cwd' <- (reverse.takeWhile (/= '/').reverse) <$> getCurrentDirectory
  createDirectory ".velle"
  putStrLn ("Creating .velle..." #OK)
  -- appendFile ".gitignore" "\n.velle/"
  putStrLn ("Project name: " <> cwd' #Name)
  appendFile ".velle/main.cfg" ("project {\n  name = \""<>cwd'<>"\"\n}\ncommands {\n\n}")
  putStrLn ("Velle init !" #Success)
