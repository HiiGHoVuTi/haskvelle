
module Utils (
  (<|), (?:), (&),
  getConf, getConfigPropFromFolder,
  ioSafe
             ) where

import Control.Exception
import qualified Data.Aeson as A
import Data.Configurator
import Data.Configurator.Types
import System.Directory
import Data.HashMap.Strict hiding (map, filter)
import qualified Data.List (isPrefixOf)
import Data.Text (pack)

infixl 7 &
(&) :: (a -> b) -> (b -> c) -> a -> c
(&) f g = g . f

infixl 2 <|
(<|) :: a -> (a -> b) -> b
(<|) x f = f x

infix 5 ?:
(?:) :: Maybe a -> a -> a
(?:) Nothing  x = x
(?:) (Just x) _ = x

getConf :: String -> IO (HashMap Name Value)
getConf dir = do
  files <- listDirectory dir
  cfg <- load . map (Optional . reverse) . filter (Data.List.isPrefixOf "gfc.") . map (reverse . ((dir<>"/") <>)) $ files
  getMap cfg


getConfigPropFromFolder :: Configured a => String -> String -> IO (Maybe a)
getConfigPropFromFolder dir prop = do
  map' <- getConf dir
  let cmd' = map' !? pack prop
      cmd  = cmd' >>= convert
    in return cmd


ioSafe :: IO A.Value -> IO A.Value
ioSafe io = let
     try' :: IO b -> IO (Either SomeException b) ; try' = try
  in io
    <| try'
    <| fmap (?. A.Null)
  where
    infixl 2 ?.
    (?.) (Left  _) a = a
    (?.) (Right a) _ = a
