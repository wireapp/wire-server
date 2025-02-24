module Test.FeatureFlags.SearchVisibilityAvailable where

import qualified API.Galley as Public
import qualified API.GalleyInternal as Internal
import SetupHelpers
import Test.FeatureFlags.Util
import Testlib.Prelude

testPatchSearchVisibility :: (HasCallStack) => FeatureTable -> App ()
testPatchSearchVisibility table = checkPatchWithTable table OwnDomain "searchVisibility" enabled

testSearchVisibilityDisabledByDefault :: (HasCallStack) => FeatureTable -> App ()
testSearchVisibilityDisabledByDefault table = do
  withModifiedBackend def {galleyCfg = setField "settings.featureFlags.teamSearchVisibility" "disabled-by-default"} $ \domain -> do
    (owner, tid, m : _) <- createTeam domain 2
    updateMigrationState domain tid table
    -- Test default
    checkFeature "searchVisibility" m tid disabled
    assertSuccess =<< Internal.setTeamFeatureStatus owner tid "searchVisibility" "enabled"
    checkFeature "searchVisibility" owner tid enabled
    assertSuccess =<< Internal.setTeamFeatureStatus owner tid "searchVisibility" "disabled"
    checkFeature "searchVisibility" owner tid disabled

testSearchVisibilityEnabledByDefault :: (HasCallStack) => FeatureTable -> App ()
testSearchVisibilityEnabledByDefault table = do
  withModifiedBackend def {galleyCfg = setField "settings.featureFlags.teamSearchVisibility" "enabled-by-default"} $ \domain -> do
    (owner, tid, m : _) <- createTeam domain 2
    updateMigrationState domain tid table
    nonMember <- randomUser domain def
    assertForbidden =<< Public.getTeamFeature nonMember tid "searchVisibility"
    -- Test default
    checkFeature "searchVisibility" m tid enabled
    assertSuccess =<< Internal.setTeamFeatureStatus owner tid "searchVisibility" "disabled"
    checkFeature "searchVisibility" owner tid disabled
    assertSuccess =<< Internal.setTeamFeatureStatus owner tid "searchVisibility" "enabled"
    checkFeature "searchVisibility" owner tid enabled
