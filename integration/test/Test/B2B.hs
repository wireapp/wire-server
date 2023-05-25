module Test.B2B where

-- import Imports
import SetupHelpers
import Testlib.Prelude

testConnectUsers :: App ()
testConnectUsers = do
  _alice <- randomUser OwnDomain def
  pure ()
