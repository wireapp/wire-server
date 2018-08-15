module Main (main) where

import Cannon.API
import Cannon.Options
import Util.Options

main :: IO ()
main = getOptions desc (Just optsParser) defaultPath >>= run
  where
    desc = "Cannon - Websocket Push Service"
    defaultPath = "/etc/wire/cannon/conf/cannon.yaml"
