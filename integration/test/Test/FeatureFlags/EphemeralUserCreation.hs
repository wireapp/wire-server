module Test.FeatureFlags.EphemeralUserCreation where

import Test.FeatureFlags.Util
import Testlib.Prelude

testEphemeralUserCreation :: (HasCallStack) => APIAccess -> App ()
testEphemeralUserCreation access =
  mkFeatureTests "ephemeralUserCreation"
    & addUpdate (object ["status" .= "disabled"])
    & addUpdate (object ["status" .= "enabled"])
    & addInvalidUpdate (object ["status" .= "INVALID"])
    & runFeatureTests OwnDomain access
