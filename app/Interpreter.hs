{-# LANGUAGE OverloadedStrings #-}

module Interpreter (
  interpret
                   ) where

import Control.Exception.Base
import qualified Data.List
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T
import Scripting.Duktape
import System.Process
import System.Directory
import Turtle hiding ((&))
import Data.Aeson
import qualified Colors
import Colors hiding (Error, Success)
import Utils


stringPipeVoid :: (String -> IO ()) -> Value -> IO Value
stringPipeVoid fn (String val) = do
  out <- val <| T.unpack
    <| fn
    <| try :: IO (Either SomeException ())
  return $ case out of
    Right _ -> (Bool True)
    _       -> (Bool False)
stringPipeVoid _ _ = return Null

stringPipeTwiceVoid :: (String -> String -> IO ()) -> Value -> Value -> IO Value
stringPipeTwiceVoid fn (String v1) (String v2) = do
  let
    arg1 = T.unpack v1
    arg2 = T.unpack v2
  out <- fn arg1 arg2
    <| try :: IO (Either SomeException ())
  return $ case out of
    Right _ -> (Bool True)
    _       -> (Bool False)
stringPipeTwiceVoid _ _ _ = return Null

mapJSON :: (FromJSON a, ToJSON b) => (a -> IO b) -> Value -> IO Value
mapJSON fn val =
  fn
      <$> fromJSON val
      <| (fmap.fmap) toJSON
      ?. return Null
      <| ioSafe
  where
    infixl 2 ?.
    (?.) (Error   _) a = a
    (?.) (Success a) _ = a

mapJSON2 :: (FromJSON a, FromJSON b, ToJSON c) => (a -> b -> IO c) -> Value -> Value -> IO Value
mapJSON2 fn v1 v2 =
  fn
      <$> fromJSON v1 <*> fromJSON v2
      <| (fmap.fmap) toJSON
      ?. return Null
      <| ioSafe
  where
    infixl 2 ?.
    (?.) (Error   _) a = a
    (?.) (Success a) _ = a


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
exec' = stringPipeVoid callCommand

readLocalConfig' :: Value -> IO Value
readLocalConfig' (String val) = do
  res <- val
    <| T.unpack
    <| getConfigPropFromFolder ".velle" :: IO (Maybe String)
  res ?: ""
    <| T.pack <| String
    <| return

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
readUserConfig' _ = return Null

mkdir' :: Value -> IO Value
mkdir' = stringPipeVoid (mkdir . decodeString)

copydir' :: Value -> Value -> IO Value
copydir' = stringPipeTwiceVoid $ \a b -> cptree (decodeString a) (decodeString b)

rmdir' :: Value -> IO Value
rmdir' = stringPipeVoid (rmtree . decodeString)

lsdir' :: Value -> IO Value
lsdir' = mapJSON listDirectory
-- lsdir recur

mkfile' :: Value -> IO Value
mkfile' = stringPipeVoid (touch . decodeString)

copyfile' :: Value -> Value -> IO Value
copyfile' = stringPipeTwiceVoid $ \a b -> cp (decodeString a) (decodeString b)

rmfile' :: Value -> IO Value
rmfile' = stringPipeVoid (rm . decodeString)

appendfile' :: Value -> Value -> IO Value
appendfile' = stringPipeTwiceVoid appendFile

readfile' :: Value -> IO Value
readfile' = mapJSON readFile

writefile' :: Value -> Value -> IO Value
writefile' = stringPipeTwiceVoid writeFile

readJSON' :: Value -> IO Value
readJSON' = mapJSON readJSON''
  where
    readJSON'' :: String -> IO Value
    readJSON''
      = B.readFile
      & fmap decode
      & fmap (?: Null)

writeJSON' :: Value -> Value -> IO Value
writeJSON' = mapJSON2 writeJSON''
  where
    writeJSON'' :: String -> Value -> IO ()
    writeJSON'' dest obj = B.writeFile dest $ encode obj

readline' :: Value -> IO Value
readline' _ = do
  res <- getLine
  return (String . T.pack$ res)

exposeAll :: DuktapeCtx -> IO ()
exposeAll duk = do
  _ <- exposeFnDuktape duk Nothing                    (C8.pack            "print") print'
  _ <- exposeFnDuktape duk (Just. C8.pack$ "console") (C8.pack              "log") log' -- ????
  _ <- exposeFnDuktape duk Nothing                    (C8.pack              "log") log'
  _ <- exposeFnDuktape duk Nothing                    (C8.pack         "readline") readline'
  _ <- exposeFnDuktape duk Nothing                    (C8.pack             "exec") exec'
  _ <- exposeFnDuktape duk Nothing                    (C8.pack  "readLocalConfig") readLocalConfig'
  _ <- exposeFnDuktape duk Nothing                    (C8.pack   "readUserConfig") readUserConfig'
  _ <- exposeFnDuktape duk Nothing                    (C8.pack            "mkdir") mkdir'
  _ <- exposeFnDuktape duk Nothing                    (C8.pack            "rmdir") rmdir'
  _ <- exposeFnDuktape duk Nothing                    (C8.pack           "mkfile") mkfile'
  _ <- exposeFnDuktape duk Nothing                    (C8.pack           "rmfile") rmfile'
  _ <- exposeFnDuktape duk Nothing                    (C8.pack            "cpdir") copydir'
  _ <- exposeFnDuktape duk Nothing                    (C8.pack           "cpfile") copyfile'
  _ <- exposeFnDuktape duk Nothing                    (C8.pack     "appendToFile") appendfile'
  _ <- exposeFnDuktape duk Nothing                    (C8.pack      "writeToFile") writefile'
  _ <- exposeFnDuktape duk Nothing                    (C8.pack               "ls") lsdir'
  _ <- exposeFnDuktape duk Nothing                    (C8.pack         "readFile") readfile'
  _ <- exposeFnDuktape duk Nothing                    (C8.pack         "readJSON") readJSON'
  _ <- exposeFnDuktape duk Nothing                    (C8.pack        "writeJSON") writeJSON'
  return ()

interpret :: String -> IO ()
interpret str = do
  dukm <- createDuktapeCtx
  case dukm of
    Nothing   -> putStrLn ("Couldn't initialise Duktape." #Colors.Error)
    Just duk  -> do
      _ <- exposeAll duk
      retVal <- evalDuktape duk $ C8.pack str
      case retVal of
        Left er        -> putStrLn $ "Duktape Error:\n" #Colors.Error <> er
        Right Nothing  -> return()
        Right (Just _) -> return()
  return ()
