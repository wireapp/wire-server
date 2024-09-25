module Test.FeatureFlags.SndFactorPasswordChallenge where

import Test.FeatureFlags.Util
import Testlib.Prelude

testPatchSndFactorPasswordChallenge :: (HasCallStack) => App ()
testPatchSndFactorPasswordChallenge =
  checkPatch OwnDomain "sndFactorPasswordChallenge" True disabledLocked enabled

testSndFactorPasswordChallenge :: (HasCallStack) => APIAccess -> App ()
testSndFactorPasswordChallenge access =
  do
    mkFeatureTests "sndFactorPasswordChallenge" disabledLocked
    & addUpdate enabled
    & addUpdate disabled
    & runFeatureTests OwnDomain access
