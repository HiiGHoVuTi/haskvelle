
module Config (
  config
              ) where

import System.Console.StructuredCLI
import Data.Configurator
import System.Directory
import Data.Text (unpack)
import Data.HashMap.Strict hiding (map)
import Data.List
import Colors

config' :: Commands ()
config' = colorCommand "config" "goes to the config menu" $ return NewLevel

configDisplay :: Commands ()
configDisplay = colorCommand "display" "displays the whole config file as a list of key/value pairs" $ do
        files <- listDirectory ".velle"
        cfg <- load . map Optional . map (".velle/" <>) $ files
        map' <- getMap cfg
        prettyPrint map'
        return NoAction

configDisplayGroup :: Commands ()
configDisplayGroup = colorCustom "display-namespace" "displays a single namespace in the config" $ \x -> do
        files <- listDirectory ".velle"
        cfg <- load . map Optional . map (".velle/" <>) $ files
        map' <- let
          keyPrefix    = head x
          map''        = getMap cfg
          in filterWithKey (\k _ -> isPrefixOf keyPrefix . unpack$ k) <$> map''
        prettyPrint map'
        return NoAction

config :: Commands () -> Commands ()
config others = config' >+ do
  others
  configDisplayGroup
  configDisplay
