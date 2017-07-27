module Main (main) where

import Brig.API
import OpenSSL (withOpenSSL)

main :: IO ()
main = withOpenSSL $ parseOptions >>= runServer
