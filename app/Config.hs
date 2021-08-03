
module Config (
  config
              ) where

import System.Console.StructuredCLI
import System.Directory
import Data.Text (unpack)
import Data.HashMap.Strict hiding (map, filter)
import qualified Data.List (isPrefixOf)
import Colors

import Utils

config' :: Commands ()
config' = colorCommand "config" "goes to the config menu" $ return NewLevel

configDisplay :: Commands ()
configDisplay = colorCommand "display" "displays the whole config file as a list of key/value pairs" $ do
        map' <- getConf ".velle"
        prettyPrint map'
        return NoAction

configDisplayGroup :: Commands ()
configDisplayGroup = colorCustom "display-namespace" "displays a single namespace in the config" $ \x -> do
        map' <- let
          keyPrefix    = head x
          map''        = getConf ".velle"
          in filterWithKey (\k _ -> Data.List.isPrefixOf keyPrefix . unpack$ k) <$> map''
        prettyPrint map'
        return NoAction

globalConfigGroup :: Commands ()
globalConfigGroup = colorCustom "display-global" "displays a namespace from the global config" $ \x -> do
        dir <- getAppUserDataDirectory "velle"
        map' <- let
          keyPrefix    = head x
          map''        = getConf dir
          in filterWithKey (\k _ -> Data.List.isPrefixOf keyPrefix . unpack$ k) <$> map''
        prettyPrint map'
        return NoAction

config :: Commands () -> Commands ()
config others = config' >+ do
  others
  configDisplayGroup
  configDisplay
  globalConfigGroup
