module Main (main) where

import Imports
import Stern.API
import Util.Options

main :: IO ()
main = getOptions desc Nothing defaultPath >>= start
  where
    desc = "Stern - Backoffice Service"
    defaultPath = "./stern.example.yaml"
