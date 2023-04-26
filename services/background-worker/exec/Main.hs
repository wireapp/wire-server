module Main where

import Imports
import OpenSSL (withOpenSSL)
import Util.Options
import Wire.BackendNotificationPusher

main :: IO ()
main = withOpenSSL $ do
  let desc = "Backend Notification Pusher"
      defaultPath = "/etc/wire/background-worker/conf/background-worker.yaml"
  options <- getOptions desc Nothing defaultPath
  run options
