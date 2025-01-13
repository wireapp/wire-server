module Main where

import Imports
import Spec qualified
import Test.Hspec

main :: IO ()
main = hspec Spec.spec
