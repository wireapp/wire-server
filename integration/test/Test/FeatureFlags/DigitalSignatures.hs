module Test.FeatureFlags.DigitalSignatures where

import SetupHelpers
import Test.FeatureFlags.Util
import Testlib.Prelude

testPatchDigitalSignatures :: (HasCallStack) => FeatureTable -> App ()
testPatchDigitalSignatures table = checkPatchWithTable table OwnDomain "digitalSignatures" enabled

testDigitalSignaturesInternal :: (HasCallStack) => FeatureTable -> App ()
testDigitalSignaturesInternal table = do
  (alice, tid, _) <- createTeam OwnDomain 0
  updateMigrationState OwnDomain tid table
  withWebSocket alice $ \ws -> do
    setFlag InternalAPI ws tid "digitalSignatures" disabled
    setFlag InternalAPI ws tid "digitalSignatures" enabled
