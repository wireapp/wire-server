module Main (main) where

import Imports
import Gundeck.API
import OpenSSL (withOpenSSL)

import Util.Options

main :: IO ()
main = withOpenSSL $ do
    options <- getOptions desc Nothing defaultPath
    runServer options
  where
    desc = "Gundeck - Push Notification Hub Service"
    defaultPath = "/etc/wire/gundeck/conf/gundeck.yaml"
