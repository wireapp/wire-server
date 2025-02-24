module Test.FeatureFlags.SearchVisibilityInbound where

import qualified API.Galley as Public
import SetupHelpers
import Test.FeatureFlags.Util
import Testlib.Prelude

testSearchVisibilityInboundInternal :: (HasCallStack) => FeatureTable -> APIAccess -> App ()
testSearchVisibilityInboundInternal table access = do
  let featureName = "searchVisibilityInbound"
  (alice, tid, _) <- createTeam OwnDomain 2
  updateMigrationState OwnDomain tid table
  eve <- randomUser OwnDomain def
  assertForbidden =<< Public.getTeamFeature eve tid featureName
  checkFeature featureName alice tid disabled

  void $ withWebSocket alice $ \ws -> do
    setFlag access ws tid featureName enabled
    setFlag access ws tid featureName disabled
