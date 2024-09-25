module Test.FeatureFlags.OutlookCalIntegration where

import Test.FeatureFlags.Util
import Testlib.Prelude

testPatchOutlookCalIntegration :: (HasCallStack) => App ()
testPatchOutlookCalIntegration = checkPatch OwnDomain "outlookCalIntegration" True disabledLocked enabled

testOutlookCalIntegration :: (HasCallStack) => APIAccess -> App ()
testOutlookCalIntegration access =
  mkFeatureTests "outlookCalIntegration" disabledLocked
    & addUpdate enabled
    & addUpdate disabled
    & runFeatureTests OwnDomain access
