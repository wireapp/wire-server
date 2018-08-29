module Main (main) where

import Brig.API
import OpenSSL (withOpenSSL)

import Brig.Options
import Util.Options

main :: IO ()
main = withOpenSSL $ do
  let desc = "Brig - User Service"
      defaultPath = "/etc/wire/brig/conf/brig.yaml"
  options <- getOptions desc (Just optsParser) defaultPath
  runServer options
