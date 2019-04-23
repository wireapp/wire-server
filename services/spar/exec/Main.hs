module Main where

import Imports
import OpenSSL (withOpenSSL)
import Spar.Run
import Spar.Options

main :: IO ()
main = withOpenSSL $ runServer =<< getOpts
