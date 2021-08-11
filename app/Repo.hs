
module Repo (
  local, load
            ) where

import Control.Exception
import Turtle hiding (find, match)
-- import System.Console.StructuredCLI
import CLI
import System.Process
import System.Directory
import qualified Data.List
import Interpreter
import Colors
import Utils

local' :: Atom
local' = colorCommand "local" "manage local repos" $ return $ return ()

existsWithWildCare :: String -> String -> IO (Maybe String)
existsWithWildCare path looked = do
  subpaths <- getSubs path
  subsubpaths <- subpaths
    |> map getSubs
    |> sequence
  concat subsubpaths
    |> Data.List.find (Data.List.isSuffixOf looked)
    |> return
  where
    getSubs p = map ((p<>"/") <>) <$> listDirectory p


loadFromGithubToFolder :: String -> String -> IO ()
loadFromGithubToFolder src dist = do
  let url = "https://github.com/" <> src <> ".git"
  tmp <- getTemporaryDirectory
  let tmp_path = tmp <> "/" <> src
  _ <- callCommand $ "git clone " <> url <> " " <> tmp_path <> " --depth 1"
  _ <- rmtree$ decodeString$ tmp_path <> "/" <> ".git"
  _ <- cptree (decodeString tmp_path) (decodeString dist)
  -- source <- getConfigPropFromFolder (tmp_path<>"/"<>".velle") "commands.on.loaded"
  -- _ <- (eval noImports (dist<>"/.velle") <$> source) ?: return ()
  _ <- rmtree$ decodeString tmp_path
  return ()
  where
    -- noImports = [] :: [(String, () -> IO ())]

list :: Atom
list = colorCommand "list" "list all local repos" $ \_ -> do
  dir <- getAppUserDataDirectory "velle"
  let repos = dir <> "/repos/"
  authors <- listDirectory $ repos
  repoList <- authors
    |> map (listDirectory . (repos <>))
    |> sequence
  _ <- prettyPrint repoList
  return ()

pull :: Atom
pull = colorCommand "pull" "pulls a repo from github <author>/<reponame>" $ \args -> do
  dir <- getAppUserDataDirectory "velle"
  let path = dir <> "/repos/" <> head args
  _ <- try$ path
    |> decodeString |> rmtree :: IO (Either SomeException ())
  _ <- loadFromGithubToFolder (head args) path
  return ()

load :: Atom
load = colorCommand "load" "loads a repo, either from local files, or from github" $ \args -> do
  dir <- getAppUserDataDirectory "velle"
  let dest = dir <> "/repos"
  match <- existsWithWildCare dest (head args)
  _ <- case match of
    Just path -> do
      cptree (decodeString path) (decodeString ".")
      source <- getConfigPropFromFolder (path<>"/"<>".velle") "commands.on.loaded"
      (eval ([]::[(String, () -> IO ())]) ("./.velle") <$> source) ?: return ()
    Nothing   -> loadFromGithubToFolder (head args) "."
  let loc = case match of
        Just _  -> "local"
        Nothing -> "GitHub"
  putStrLn ("Successfully loaded repo from "#Success <> loc #Name <> " !"#Success)
  return ()

save :: Atom
save = colorCommand "save" "saves the current folder as a local repo" $ \_ -> do
  dir <- getAppUserDataDirectory "velle"
  data' <- getConfigPropFromFolder dir "github.username" :: IO (Maybe String)
  username <- return$ case data' of
    (Just n) -> n
    Nothing     -> error (("Please edit "<>dir<>"/main.cfg to add your github username.") #Error)
   -- dir </> repos </> username </> current folder name
  project <- getConfigPropFromFolder ".velle" "project.name"
  path <- return$ dir <>"/repos/"<> username <>"/"<> case project of
    Just n -> n
    Nothing   -> error ("Velle isn't init in this directory." #Error)
  _ <- try$ rmtree.decodeString$ path :: IO (Either SomeException ())
  _ <- createDirectory path
  _ <- cptree (decodeString ".") (decodeString path)
  putStrLn (("Successfully saved your current project !") #Success)
  return ()

-- publish

local :: Atom
local = local' >+
  [ list
  , save
  , pull
  ]
