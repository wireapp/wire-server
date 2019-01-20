module Main (main) where

import Imports
import Test.Tasty
import ZAuth

main :: IO ()
main = defaultMain =<< tests
