{-# LANGUAGE LambdaCase #-}

module Utils (
  (|>), (?:), (&), (</>),
  getConf, getConfigPropFromFolder,
  ioSafe, voidIOSafe, voidIOSafeError
             ) where

import Control.Exception
import Control.Monad
import qualified Data.Aeson as A
import Data.Configurator
import Data.Configurator.Types
import System.Directory
import Data.HashMap.Strict hiding (map, filter)
import qualified Data.List (isPrefixOf)
import Data.Text (pack)
import Colors

-- | Function composition, reverse (.)
infixl 7 &
(&) :: (a -> b) -> (b -> c) -> a -> c
(&) f g = g . f

-- | Piping operator
infixl 2 |>
(|>) :: a -> (a -> b) -> b
(|>) x f = f x

-- | Maybe default operator
infix 5 ?:
(?:) :: Maybe a -> a -> a
(?:) Nothing  x = x
(?:) (Just x) _ = x

-- | Join paths with a "/" in between
(</>) :: String -> String -> String
(</>) a b = a <> "/" <> b

-- | Obtains the key-value map from the config directory
getConf :: String -> IO (HashMap Name Value)
getConf dir = do
  files <- listDirectory dir
  cfg <- load . map (Optional . reverse) . filter (Data.List.isPrefixOf "gfc.") . map (reverse . ((dir<>"/") <>)) $ files
  getMap cfg

-- | Obtains a single property from the config directory
getConfigPropFromFolder :: Configured a => String -> String -> IO (Maybe a)
getConfigPropFromFolder dir prop = do
  map' <- getConf dir
  let cmd' = map' !? pack prop
      cmd  = cmd' >>= convert
    in return cmd

-- | Prevents an IO action from crashing, prints the error in case of fail
voidIOSafeError :: IO () -> IO ()
voidIOSafeError io = let
     try' :: IO b -> IO (Either SomeException b) ; try' = try
  in io
     |> try'
     |> fmap (\case
                 Left (SomeException e)  -> putStrLn (show e #Error)
                 Right                 _ -> return ())
     |> join

-- | Totally ignores failure of an IO action
voidIOSafe :: IO () -> IO ()
voidIOSafe io = let
     try' = try :: IO b -> IO (Either SomeException b)
  in io
     |> try'
     |> void

-- | Replaces an IO crash by a Null return value
ioSafe :: IO A.Value -> IO A.Value
ioSafe io = let
     try' = try :: IO b -> IO (Either SomeException b)
  in io
    |> try'
    |> fmap (?. A.Null)
  where
    infixl 2 ?.
    (?.) (Left  _) a = a
    (?.) (Right a) _ = a
