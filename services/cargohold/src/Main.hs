module Main (main) where

import CargoHold.API
import OpenSSL (withOpenSSL)

import CargoHold.Options
import Util.Options

main :: IO ()
main = withOpenSSL $ do
    options <- getOptions desc optsParser defaultPath
    runServer options
  where
    desc = "Cargohold - Asset Storage"
    defaultPath = "/etc/wire/cargohold/conf/cargohold.yaml"
