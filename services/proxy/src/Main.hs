module Main (main) where

import Proxy.API
import Proxy.Options
import Util.Options

main :: IO ()
main = do
    opts <- getOptions desc optsParser defaultPath
    run opts
  where
    desc = "Proxy - 3rd party proxy"
    defaultPath = "/etc/wire/proxy/conf/proxy.yaml"
