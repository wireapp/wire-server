module Main (main) where

import Cannon.API
import Cannon.Types

main :: IO ()
main = parseOptions >>= run
