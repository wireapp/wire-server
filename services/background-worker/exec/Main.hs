module Main where

import Imports
import OpenSSL (withOpenSSL)
import Util.Options
import Wire.BackgroundWorker

main :: IO ()
main = withOpenSSL $ do
  let desc = "Background Worker"
      defaultPath = "/etc/wire/background-worker/conf/background-worker.yaml"
  options <- getOptions desc Nothing defaultPath
  run options
