module Main (main) where

import Imports
import Cannon.API
import Util.Options

main :: IO ()
main = getOptions desc Nothing defaultPath >>= run
  where
    desc = "Cannon - Websocket Push Service"
    defaultPath = "/etc/wire/cannon/conf/cannon.yaml"
