module Main (main) where

import Imports
import CargoHold.API
import CargoHold.Options
import Util.Options

main :: IO ()
main = getOptions desc (Just optsParser) defaultPath >>= runServer
  where
    desc = "Cargohold - Asset Storage"
    defaultPath = "/etc/wire/cargohold/conf/cargohold.yaml"
