module Test.FeatureFlags.SndFactorPasswordChallenge where

import Test.FeatureFlags.Util
import Testlib.Prelude

testPatchSndFactorPasswordChallenge :: (HasCallStack) => FeatureTable -> App ()
testPatchSndFactorPasswordChallenge table =
  checkPatchWithTable table OwnDomain "sndFactorPasswordChallenge" enabled

testSndFactorPasswordChallenge :: (HasCallStack) => FeatureTable -> APIAccess -> App ()
testSndFactorPasswordChallenge table access =
  do
    mkFeatureTests "sndFactorPasswordChallenge"
    & addUpdate enabled
    & addUpdate disabled
    & setTable table
    & runFeatureTests OwnDomain access
