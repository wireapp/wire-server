module Test.FeatureFlags.AppLock where

import qualified Data.Aeson as A
import Test.FeatureFlags.Util
import Testlib.Prelude

testPatchAppLock :: (HasCallStack) => App ()
testPatchAppLock = do
  let defCfg =
        object
          [ "lockStatus" .= "unlocked",
            "status" .= "enabled",
            "ttl" .= "unlimited",
            "config" .= object ["enforceAppLock" .= False, "inactivityTimeoutSecs" .= A.Number 60]
          ]
  checkPatch OwnDomain "appLock" False defCfg (object ["lockStatus" .= "locked"])
  checkPatch OwnDomain "appLock" False defCfg (object ["status" .= "disabled"])
  checkPatch OwnDomain "appLock" False defCfg (object ["lockStatus" .= "locked", "status" .= "disabled"])
  checkPatch OwnDomain "appLock" False defCfg (object ["lockStatus" .= "unlocked", "config" .= object ["enforceAppLock" .= True, "inactivityTimeoutSecs" .= A.Number 120]])
  checkPatch OwnDomain "appLock" False defCfg (object ["config" .= object ["enforceAppLock" .= True, "inactivityTimeoutSecs" .= A.Number 240]])
