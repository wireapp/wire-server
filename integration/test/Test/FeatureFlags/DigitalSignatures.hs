module Test.FeatureFlags.DigitalSignatures where

import SetupHelpers
import Test.FeatureFlags.Util
import Testlib.Prelude

testPatchDigitalSignatures :: (HasCallStack) => App ()
testPatchDigitalSignatures = checkPatch OwnDomain "digitalSignatures" False disabled enabled

testDigitalSignaturesInternal :: (HasCallStack) => App ()
testDigitalSignaturesInternal = do
  (alice, tid, _) <- createTeam OwnDomain 0
  withWebSocket alice $ \ws -> do
    setFlag InternalAPI ws tid "digitalSignatures" disabled
    setFlag InternalAPI ws tid "digitalSignatures" enabled
