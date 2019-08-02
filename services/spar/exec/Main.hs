module Main where

import Imports
import Spar.Run
import Spar.Options

main :: IO ()
main = runServer =<< getOpts
