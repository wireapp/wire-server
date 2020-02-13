module Main (main) where

import Gundeck.Run (run)
import Imports
import OpenSSL (withOpenSSL)
import Util.Options

main :: IO ()
main = withOpenSSL $ do
  options <- getOptions desc Nothing defaultPath
  run options
  where
    desc = "Gundeck - Push Notification Hub Service"
    defaultPath = "/etc/wire/gundeck/conf/gundeck.yaml"
