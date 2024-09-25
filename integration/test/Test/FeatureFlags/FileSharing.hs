module Test.FeatureFlags.FileSharing where

import Test.FeatureFlags.Util
import Testlib.Prelude

testPatchFileSharing :: (HasCallStack) => App ()
testPatchFileSharing = checkPatch OwnDomain "fileSharing" True enabled disabled

testFileSharing :: (HasCallStack) => APIAccess -> App ()
testFileSharing access =
  mkFeatureTests "fileSharing" enabled
    & addUpdate disabled
    & addUpdate enabled
    & runFeatureTests OwnDomain access
