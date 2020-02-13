module Main (main) where

import Imports
import Proxy.Run (run)
import Util.Options

main :: IO ()
main = do
  opts <- getOptions desc Nothing defaultPath
  run opts
  where
    desc = "Proxy - 3rd party proxy"
    defaultPath = "/etc/wire/proxy/conf/proxy.yaml"
