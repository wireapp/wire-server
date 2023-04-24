module Main where

import Imports
import OpenSSL (withOpenSSL)
import Util.Options
import Wire.BackendNotificationPusher

main :: IO ()
main = withOpenSSL $ do
  let desc = "Backend Notification Pusher"
      defaultPath = "/etc/wire/backend-notification-pusher/conf/backend-notification-pusher.yaml"
  options <- getOptions desc Nothing defaultPath
  run options
