module Test.B2B where

import qualified API.Brig as Public
import Control.Lens
import Data.Aeson
import Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Lens
import SetupHelpers
import Testlib.Prelude

testConnectUsers :: App ()
testConnectUsers = do
  _alice <- randomUser OwnDomain def
  pure ()
