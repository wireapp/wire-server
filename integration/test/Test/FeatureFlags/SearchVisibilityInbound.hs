module Test.FeatureFlags.SearchVisibilityInbound where

import qualified API.Galley as Public
import qualified API.GalleyInternal as Internal
import SetupHelpers
import Test.FeatureFlags.Util
import Testlib.Prelude

testFeatureNoConfigMultiSearchVisibilityInbound :: (HasCallStack) => App ()
testFeatureNoConfigMultiSearchVisibilityInbound = do
  (_owner1, team1, _) <- createTeam OwnDomain 0
  (_owner2, team2, _) <- createTeam OwnDomain 0

  assertSuccess =<< Internal.setTeamFeatureStatus OwnDomain team2 "searchVisibilityInbound" "enabled"

  response <- Internal.getFeatureStatusMulti OwnDomain "searchVisibilityInbound" [team1, team2]

  statuses <- response.json %. "default_status" >>= asList
  length statuses `shouldMatchInt` 2
  statuses `shouldMatchSet` [object ["team" .= team1, "status" .= "disabled"], object ["team" .= team2, "status" .= "enabled"]]

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
