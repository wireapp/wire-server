module Test.FeatureFlags.SearchVisibilityInbound where

import qualified API.Galley as Public
import SetupHelpers
import Test.FeatureFlags.Util
import Testlib.Prelude

testSearchVisibilityInboundInternal :: (HasCallStack) => APIAccess -> App ()
testSearchVisibilityInboundInternal access = do
  let featureName = "searchVisibilityInbound"
  (alice, tid, _) <- createTeam OwnDomain 2
  eve <- randomUser OwnDomain def
  assertForbidden =<< Public.getTeamFeature eve tid featureName
  checkFeature featureName alice tid disabled

  void $ withWebSocket alice $ \ws -> do
    setFlag access ws tid featureName enabled
    setFlag access ws tid featureName disabled
