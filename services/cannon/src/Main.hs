module Main (main) where

import Imports
import Cannon.Run (run)
import Util.Options

main :: IO ()
main = getOptions desc Nothing defaultPath >>= run
  where
    desc = "Cannon - Websocket Push Service"
    defaultPath = "/etc/wire/cannon/conf/cannon.yaml"
