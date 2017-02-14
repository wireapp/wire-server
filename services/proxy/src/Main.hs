module Main (main) where

import Proxy.API
import Proxy.Options (parseOptions)

main :: IO ()
main = parseOptions >>= run
