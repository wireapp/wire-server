module Main (main) where

import CargoHold.API
import OpenSSL

main :: IO ()
main = withOpenSSL $ parseOptions >>= start
