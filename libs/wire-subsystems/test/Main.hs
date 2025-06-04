module Main where

import Imports
import qualified Spec
import qualified Test.Hspec as H

main :: IO ()
main = H.hspec Spec.spec
