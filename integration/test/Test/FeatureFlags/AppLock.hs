module Test.FeatureFlags.AppLock where

import qualified Data.Aeson as A
import Test.FeatureFlags.Util
import Testlib.Prelude

testPatchAppLock :: (HasCallStack) => App ()
testPatchAppLock = do
  checkPatch OwnDomain "appLock"
    $ object ["lockStatus" .= "locked"]
  checkPatch OwnDomain "appLock"
    $ object ["status" .= "disabled"]
  checkPatch OwnDomain "appLock"
    $ object ["lockStatus" .= "locked", "status" .= "disabled"]
  checkPatch OwnDomain "appLock"
    $ object
      [ "lockStatus" .= "unlocked",
        "config"
          .= object
            [ "enforceAppLock" .= True,
              "inactivityTimeoutSecs" .= A.Number 120
            ]
      ]
  checkPatch OwnDomain "appLock"
    $ object
      [ "config"
          .= object
            [ "enforceAppLock" .= True,
              "inactivityTimeoutSecs" .= A.Number 240
            ]
      ]
