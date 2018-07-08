module Main (main) where

import Galley.API
import Galley.Options
import OpenSSL (withOpenSSL)
import Util.Options

main :: IO ()
main = withOpenSSL $ do
  let desc = "Galley - Conversation service"
      defaultPath = "/etc/wire/galley/conf/galley.yaml"
  options <- getOptions desc optsParser defaultPath
  run options
