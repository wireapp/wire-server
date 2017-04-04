module Main (main) where

import Galley.API
import Galley.Options
import OpenSSL

main :: IO ()
main = withOpenSSL $ parseOptions >>= run

