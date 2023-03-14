module Test.Client where

import App
import Data.Default
import Imports
import Setup

testCantDeleteLHClient :: App ()
testCantDeleteLHClient = do
  user <- randomUser def
  print user
