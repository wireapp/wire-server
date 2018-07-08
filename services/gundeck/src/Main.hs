module Main (main) where

import Gundeck.API
import OpenSSL (withOpenSSL)

import Gundeck.Options
import Util.Options

main :: IO ()
main = withOpenSSL $ do
    options <- getOptions desc optsParser defaultPath
    runServer options
  where
    desc = "Gundeck - Push Notification Hub Service"
    defaultPath = "/etc/wire/gundeck/conf/gundeck.yaml"
