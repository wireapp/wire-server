module Main where

import Imports
import Network.DNS.Resolver
import Network.Federation.Util

main :: IO ()
main = do
  rs <- makeResolvSeed defaultResolvConf
  x <- srvLookup "staging.zinfra.io" rs
  print x
