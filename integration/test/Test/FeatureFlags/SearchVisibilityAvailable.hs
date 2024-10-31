module Test.FeatureFlags.SearchVisibilityAvailable where

import qualified API.Galley as Public
import qualified API.GalleyInternal as Internal
import SetupHelpers
import Test.FeatureFlags.Util
import Testlib.Prelude

testPatchSearchVisibility :: (HasCallStack) => App ()
testPatchSearchVisibility = checkPatch OwnDomain "searchVisibility" enabled

testSearchVisibilityDisabledByDefault :: (HasCallStack) => App ()
testSearchVisibilityDisabledByDefault = do
  withModifiedBackend def {galleyCfg = setField "settings.featureFlags.teamSearchVisibility" "disabled-by-default"} $ \domain -> do
    (owner, tid, m : _) <- createTeam domain 2
    -- Test default
    checkFeature "searchVisibility" m tid disabled
    assertSuccess =<< Internal.setTeamFeatureStatus owner tid "searchVisibility" "enabled"
    checkFeature "searchVisibility" owner tid enabled
    assertSuccess =<< Internal.setTeamFeatureStatus owner tid "searchVisibility" "disabled"
    checkFeature "searchVisibility" owner tid disabled

testSearchVisibilityEnabledByDefault :: (HasCallStack) => App ()
testSearchVisibilityEnabledByDefault = do
  withModifiedBackend def {galleyCfg = setField "settings.featureFlags.teamSearchVisibility" "enabled-by-default"} $ \domain -> do
    (owner, tid, m : _) <- createTeam domain 2
    nonMember <- randomUser domain def
    assertForbidden =<< Public.getTeamFeature nonMember tid "searchVisibility"
    -- Test default
    checkFeature "searchVisibility" m tid enabled
    assertSuccess =<< Internal.setTeamFeatureStatus owner tid "searchVisibility" "disabled"
    checkFeature "searchVisibility" owner tid disabled
    assertSuccess =<< Internal.setTeamFeatureStatus owner tid "searchVisibility" "enabled"
    checkFeature "searchVisibility" owner tid enabled
