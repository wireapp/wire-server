module Main (main) where

import CargoHold.API
import CargoHold.Options
import Util.Options

main :: IO ()
main = getOptions desc optsParser defaultPath >>= runServer
  where
    desc = "Cargohold - Asset Storage"
    defaultPath = "/etc/wire/cargohold/conf/cargohold.yaml"
