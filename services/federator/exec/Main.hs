module Main (main) where

import Federator.Run (run)
import Imports
import Util.Options (getOptions)

main :: IO ()
main = do
  let desc = "Federation Service"
      defaultPath = "/etc/wire/federator/conf/federator.yaml"
  options <- getOptions desc Nothing defaultPath
  run options
