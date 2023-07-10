module Test.B2B where

import SetupHelpers
import Testlib.Prelude

testConnectUsers :: App ()
testConnectUsers = do
  _alice <- randomUser OwnDomain def
  pure ()
