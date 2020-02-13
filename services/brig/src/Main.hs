module Main (main) where

import Brig.Run (run)
import Imports
import OpenSSL (withOpenSSL)
import Util.Options

main :: IO ()
main = withOpenSSL $ do
  let desc = "Brig - User Service"
      defaultPath = "/etc/wire/brig/conf/brig.yaml"
  options <- getOptions desc Nothing defaultPath
  run options
