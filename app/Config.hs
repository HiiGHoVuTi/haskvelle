
module Config (
  config
              ) where

-- import System.Console.StructuredCLI
import CLI
import System.Directory
import Data.Text (unpack)
import Data.HashMap.Strict hiding (map, filter)
import qualified Data.List (isPrefixOf)
import Colors

import Utils

config' :: Atom
config' = colorCommand "config" "goes to the config menu" $ return $ return ()

configDisplay :: Atom
configDisplay = colorCommand "display" "displays the whole config file as a list of key/value pairs" $ \_ ->do
        map' <- getConf ".velle"
        prettyPrint map'
        return ()

configDisplayGroup :: Atom
configDisplayGroup = colorCommand "display-namespace" "displays a single namespace in the config" $ \x -> do
        map' <- let
          keyPrefix    = head x
          map''        = getConf ".velle"
          in filterWithKey (\k _ -> Data.List.isPrefixOf keyPrefix . unpack$ k) <$> map''
        prettyPrint map'
        return ()

globalConfigGroup :: Atom
globalConfigGroup = colorCommand "display-global" "displays a namespace from the global config" $ \x -> do
        dir <- getAppUserDataDirectory "velle"
        map' <- let
          keyPrefix    = head x
          map''        = getConf dir
          in filterWithKey (\k _ -> Data.List.isPrefixOf keyPrefix . unpack$ k) <$> map''
        prettyPrint map'
        return ()

config :: Atom
config = config' >+
  [ configDisplayGroup
  , configDisplay
  , globalConfigGroup
  ]
