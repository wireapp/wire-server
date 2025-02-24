module Test.FeatureFlags.OutlookCalIntegration where

import Test.FeatureFlags.Util
import Testlib.Prelude

testPatchOutlookCalIntegration :: (HasCallStack) => FeatureTable -> App ()
testPatchOutlookCalIntegration table = checkPatchWithTable table OwnDomain "outlookCalIntegration" enabled

testOutlookCalIntegration :: (HasCallStack) => FeatureTable -> APIAccess -> App ()
testOutlookCalIntegration table access =
  mkFeatureTests "outlookCalIntegration"
    & addUpdate enabled
    & addUpdate disabled
    & setTable table
    & runFeatureTests OwnDomain access
