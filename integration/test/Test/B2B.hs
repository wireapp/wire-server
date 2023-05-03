module Test.B2B where

-- import Imports
import SetupHelpers
import Testlib.Prelude

testConnectUsers :: App ()
testConnectUsers = do
  alice <- randomUser def
  bob <- withTwo (randomUser def)
  connectUsersB2B alice bob
