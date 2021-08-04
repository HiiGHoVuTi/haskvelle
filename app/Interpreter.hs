{-# LANGUAGE OverloadedStrings #-}

module Interpreter (
  interpret
                   ) where

import qualified Data.List
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T
import Scripting.Duktape
import System.Process
import System.Directory
import Data.Aeson hiding (Error, Success)
import Colors
import Utils

print' :: Value -> IO Value
print' v = do
  prettyPrint v
  return$ v

log' :: Value -> IO Value
log' v = do
  v
    <| show
    <| words <| tail
    <| Data.List.intersperse " "
    <| concat
    <| tail <| reverse
    <| tail <| reverse
    <| putStrLn
  return Null

exec' :: Value -> IO Value
exec' (String val) = do
  _ <- val <| T.unpack <| callCommand
  return (Bool True)
exec' _ = return Null

readLocalConfig' :: Value -> IO Value
readLocalConfig' (String val) = do
  res <- val
    <| T.unpack
    <| getConfigPropFromFolder ".velle" :: IO (Maybe String)
  res ?: ""
    <| T.pack <| String
    <| return
  where
    (?:) Nothing  x = x
    (?:) (Just x) _ = x
readLocalConfig' _ = return Null

readUserConfig' :: Value -> IO Value
readUserConfig' (String val) = do
  dir <- getAppUserDataDirectory "velle"
  res <- val
    <| T.unpack
    <| getConfigPropFromFolder dir :: IO (Maybe String)
  res ?: ""
    <| T.pack <| String
    <| return
  where
    (?:) Nothing  x = x
    (?:) (Just x) _ = x
readUserConfig' _ = return Null


exposeAll :: DuktapeCtx -> IO ()
exposeAll duk = do
  _ <- exposeFnDuktape duk Nothing                    (C8.pack            "print") print'
  _ <- exposeFnDuktape duk (Just. C8.pack$ "console") (C8.pack              "log") log' -- ????
  _ <- exposeFnDuktape duk Nothing                    (C8.pack              "log") log'
  _ <- exposeFnDuktape duk Nothing                    (C8.pack             "exec") exec'
  _ <- exposeFnDuktape duk Nothing                    (C8.pack  "readLocalConfig") readLocalConfig'
  _ <- exposeFnDuktape duk Nothing                    (C8.pack   "readUserConfig") readUserConfig'
  return ()

interpret :: String -> IO ()
interpret s = do
  dukm <- createDuktapeCtx
  case dukm of
    Nothing   -> putStrLn ("Couldn't initialise Duktape." #Error)
    Just duk  -> do
      _ <- exposeAll duk
      retVal <- evalDuktape duk $ C8.pack s
      case retVal of
        Left e         -> putStrLn $ "Duktape Error:\n" #Error <> e
        Right Nothing  -> return()
        Right (Just _) -> return()
  return ()
