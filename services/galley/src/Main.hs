module Main (main) where

import Galley.Run (run)
import Imports
import OpenSSL (withOpenSSL)
import Util.Options

main :: IO ()
main = withOpenSSL $ do
  let desc = "Galley - Conversation service"
      defaultPath = "/etc/wire/galley/conf/galley.yaml"
  options <- getOptions desc Nothing defaultPath
  run options
