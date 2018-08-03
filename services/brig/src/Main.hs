module Main (main) where

import Brig.API
import OpenSSL (withOpenSSL)

import Util.Options

main :: IO ()
main = withOpenSSL $ do
  let desc = "Brig - User Service"
      defaultPath = "/etc/wire/brig/conf/brig.yaml"
  options <- getOptions desc Nothing defaultPath
  runServer options
