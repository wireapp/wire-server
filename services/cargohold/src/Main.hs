module Main (main) where

import Imports
import CargoHold.API
import OpenSSL (withOpenSSL)

import Util.Options

main :: IO ()
main = withOpenSSL $ do
    options <- getOptions desc Nothing defaultPath
    runServer options
  where
    desc = "Cargohold - Asset Storage"
    defaultPath = "/etc/wire/cargohold/conf/cargohold.yaml"
