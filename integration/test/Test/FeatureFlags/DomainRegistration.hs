module Test.FeatureFlags.DomainRegistration where

import qualified API.GalleyInternal as Internal
import SetupHelpers
import Test.FeatureFlags.Util
import Testlib.Prelude

testPatchDomainRegistration :: (HasCallStack) => FeatureTable -> App ()
testPatchDomainRegistration table = checkPatchWithTable table OwnDomain "domainRegistration" enabled

testDomainRegistrationInternal :: (HasCallStack) => FeatureTable -> App ()
testDomainRegistrationInternal table = do
  (alice, tid, _) <- createTeam OwnDomain 0
  updateMigrationState OwnDomain tid table
  Internal.setTeamFeatureLockStatus alice tid "domainRegistration" "unlocked"
  withWebSocket alice $ \ws -> do
    setFlag InternalAPI ws tid "domainRegistration" enabled
    setFlag InternalAPI ws tid "domainRegistration" disabled
