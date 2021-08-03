
module Repo (
  local, load
            ) where

import Turtle hiding (find, match)
import System.Console.StructuredCLI
import System.Process
import System.Directory
import qualified Data.List
import Colors
import Utils

local' :: Commands ()
local' = colorCommand "local" "manage local repos" $ return NewLevel

existsWithWildCare :: String -> String -> IO (Maybe String)
existsWithWildCare path looked = do
  subpaths <- getSubs path
  subsubpaths <- subpaths
    <| map getSubs
    <| sequence
  concat subsubpaths
    <| Data.List.find (Data.List.isSuffixOf looked)
    <| return
  where
    getSubs p = map ((p<>"/") <>) <$> listDirectory p


list :: Commands ()
list = colorCommand "list" "list all local repos" $ do
  dir <- getAppUserDataDirectory "velle"
  let repos = dir <> "/repos/"
  authors <- listDirectory $ repos
  repoList <- authors
    <| map (listDirectory . (repos <>))
    <| sequence
  _ <- prettyPrint repoList
  return NoAction

pull :: Commands ()
pull = colorCustom "pull" "pulls a repo from github <author>/<reponame>" $ \args -> do
  let url = "https://github.com/" <> head args <>".git"
  dir <- getAppUserDataDirectory "velle"
  _ <- (dir <> "/repos/")
    <| decodeString <| rmtree
  _ <- callCommand $ "git clone " <> url <> " " <> dir <> "/repos/" <> head args
  return NoAction

load :: Commands ()
load = colorCustom "load" "loads a repo, either from local files, or from github" $ \args -> do
  dir <- getAppUserDataDirectory "velle"
  let dest = dir <> "/repos"
  match <- existsWithWildCare dest (head args)
  _ <- case match of
    Just path -> cptree (decodeString path) (decodeString ".")
    Nothing   -> let
      url = "https://github.com/" <> head args <>".git"
      in callCommand $ "git clone " <> url <> " ."
  return NoAction

save :: Commands ()
save = colorCommand "save" "saves the current folder as a local repo" $ do
  dir <- getAppUserDataDirectory "velle"
  data' <- getConfigPropFromFolder dir "github.username" :: IO (Maybe String)
  username <- return$ case data' of
    (Just name) -> name
    Nothing     -> error (("Please edit "<>dir<>"/main.cfg to add your github username.") #Error)
   -- dir </> repos </> username </> current folder name
  project <- getConfigPropFromFolder ".velle" "project.name"
  path <- return$ dir <>"/repos/"<> username <>"/"<> case project of
    Just name -> name
    Nothing   -> error ("Velle isn't init in this directory." #Error)
  _ <- removeDirectoryRecursive path
  _ <- createDirectory path
  _ <- cptree (decodeString ".") (decodeString path)
  putStrLn (("Successfully saved your current project !") #Success)
  return NoAction

-- publish

local :: Commands () -> Commands ()
local others = local' >+ do
  others
  list
  load
  save
  pull
