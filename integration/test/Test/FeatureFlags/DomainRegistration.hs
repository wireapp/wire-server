module Test.FeatureFlags.DomainRegistration where

import qualified API.GalleyInternal as Internal
import SetupHelpers
import Test.FeatureFlags.Util
import Testlib.Prelude

testPatchDomainRegistration :: (HasCallStack) => App ()
testPatchDomainRegistration = checkPatch OwnDomain "domainRegistration" enabled

testDomainRegistrationInternal :: (HasCallStack) => App ()
testDomainRegistrationInternal = do
  (alice, tid, _) <- createTeam OwnDomain 0
  Internal.setTeamFeatureLockStatus alice tid "domainRegistration" "unlocked"
  withWebSocket alice $ \ws -> do
    setFlag InternalAPI ws tid "domainRegistration" enabled
    setFlag InternalAPI ws tid "domainRegistration" disabled
