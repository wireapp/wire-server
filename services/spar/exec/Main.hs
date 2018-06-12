module Main where

import Spar.API
import Spar.Options

main :: IO ()
main = runServer =<< getOpts
