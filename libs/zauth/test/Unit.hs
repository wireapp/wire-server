module Main (main) where

import Test.Tasty
import ZAuth

main :: IO ()
main = tests >>= defaultMain
