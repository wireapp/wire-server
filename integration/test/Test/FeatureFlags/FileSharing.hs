module Test.FeatureFlags.FileSharing where

import Test.FeatureFlags.Util
import Testlib.Prelude

testPatchFileSharing :: (HasCallStack) => FeatureTable -> App ()
testPatchFileSharing table = checkPatchWithTable table OwnDomain "fileSharing" disabled

testFileSharing :: (HasCallStack) => FeatureTable -> APIAccess -> App ()
testFileSharing table access =
  mkFeatureTests "fileSharing"
    & addUpdate disabled
    & addUpdate enabled
    & setTable table
    & runFeatureTests OwnDomain access
