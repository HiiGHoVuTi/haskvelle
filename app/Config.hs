
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

-- | The Node for the config menu, function does nothing
config' :: Atom
config' = colorCommand "config" "goes to the config menu" $ return.const ()

-- | Displays the config when called
configDisplay :: Atom
configDisplay = colorCommand "display" "displays the whole config file as a list of key/value pairs" $ \_ ->do
        map' <- getConf ".velle"
        prettyPrint map'
        return ()

-- | Displays a single namespace in the config when called
configDisplayGroup :: Atom
configDisplayGroup = colorCommand "display-namespace" "displays a single namespace in the config" $ \x -> do
        map' <- let
          keyPrefix    = head x
          map''        = getConf ".velle"
          in filterWithKey (\k _ -> Data.List.isPrefixOf keyPrefix . unpack$ k) <$> map''
        prettyPrint map'
        return ()

-- | Displays a single namespace from the *global* config when called
globalConfigGroup :: Atom
globalConfigGroup = colorCommand "display-global" "displays a namespace from the global config" $ \x -> do
        dir <- getAppUserDataDirectory "velle"
        map' <- let
          keyPrefix    = head x
          map''        = getConf dir
          in filterWithKey (\k _ -> Data.List.isPrefixOf keyPrefix . unpack$ k) <$> map''
        prettyPrint map'
        return ()

-- | The public Node which handles everything config related
config :: Atom -> Atom
config x = config' >+
  [ x
  , configDisplayGroup
  , configDisplay
  , globalConfigGroup
  ]
