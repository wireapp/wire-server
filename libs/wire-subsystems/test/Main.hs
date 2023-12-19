module Main where

import Imports
import Test.Hspec
import Test.Wire.Notification qualified as Notification

main :: IO ()
main = hspec do
  Notification.spec
