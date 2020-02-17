module Main where

import Imports
import Spar.Options
import Spar.Run

main :: IO ()
main = runServer =<< getOpts
