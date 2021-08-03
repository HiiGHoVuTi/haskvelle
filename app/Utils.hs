
module Utils (
  (<|),
  getConf, getConfigPropFromFolder
             ) where


import Data.Configurator
import Data.Configurator.Types
import System.Directory
import Data.HashMap.Strict hiding (map, filter)
import qualified Data.List (isPrefixOf)
import Data.Text (pack)

(<|) :: a -> (a -> b) -> b
(<|) x f = f x


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
