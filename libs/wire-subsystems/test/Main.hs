module Main where

import Imports
import Spec
import Test.Hspec

main :: IO ()
main = hspec Spec.spec
