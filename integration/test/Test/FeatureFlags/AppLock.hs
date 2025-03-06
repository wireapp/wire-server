module Test.FeatureFlags.AppLock where

import qualified Data.Aeson as A
import Test.FeatureFlags.Util
import Testlib.Prelude

testPatchAppLock :: (HasCallStack) => FeatureTable -> App ()
testPatchAppLock table = do
  checkPatchWithTable table OwnDomain "appLock"
    $ object ["lockStatus" .= "locked"]
  checkPatchWithTable table OwnDomain "appLock"
    $ object ["status" .= "disabled"]
  checkPatchWithTable table OwnDomain "appLock"
    $ object ["lockStatus" .= "locked", "status" .= "disabled"]
  checkPatchWithTable table OwnDomain "appLock"
    $ object
      [ "lockStatus" .= "unlocked",
        "config"
          .= object
            [ "enforceAppLock" .= True,
              "inactivityTimeoutSecs" .= A.Number 120
            ]
      ]
  checkPatchWithTable table OwnDomain "appLock"
    $ object
      [ "config"
          .= object
            [ "enforceAppLock" .= True,
              "inactivityTimeoutSecs" .= A.Number 240
            ]
      ]

testPatchAppLockReadOnly :: (HasCallStack) => App ()
testPatchAppLockReadOnly = do
  checkPatchReadOnly OwnDomain "appLock"
    $ object ["lockStatus" .= "locked"]
