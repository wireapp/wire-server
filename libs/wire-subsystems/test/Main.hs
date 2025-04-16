module Main where

import Imports
import Spec qualified
import Test.Hspec qualified as H

main :: IO ()
main = H.hspec Spec.spec
