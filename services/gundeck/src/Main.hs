module Main (main) where

import Imports
import Gundeck.API
import Control.Monad (when)
import Control.Lens ((^.))
import OpenSSL (withOpenSSL)

import Gundeck.Options
import Util.Options

main :: IO ()
main = withOpenSSL $ do
    options <- getOptions desc Nothing defaultPath
    when (options^.optFallback.fbQueueDelay < 30) $
        error "fbQueueDelay must be 30s or higher"
    runServer options
  where
    desc = "Gundeck - Push Notification Hub Service"
    defaultPath = "/etc/wire/gundeck/conf/gundeck.yaml"
