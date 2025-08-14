module Test.FeatureFlags.Apps where

import qualified API.GalleyInternal as Internal
import SetupHelpers
import Test.FeatureFlags.Util
import Testlib.Prelude

testAppsInternal :: (HasCallStack) => App ()
testAppsInternal = do
  (alice, tid, _) <- createTeam OwnDomain 0
  Internal.setTeamFeatureLockStatus alice tid "apps" "unlocked"
  withWebSocket alice $ \ws -> do
    setFlag InternalAPI ws tid "apps" enabled
    setFlag InternalAPI ws tid "apps" disabled
  Internal.setTeamFeatureLockStatus alice tid "apps" "locked"
  setFeature InternalAPI alice tid "apps" enabled `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 409
    resp.json %. "label" `shouldMatch` "feature-locked"
  -- the feature does not have a public PUT endpoint
  setFeature PublicAPI alice tid "apps" enabled `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 404
    resp.json %. "label" `shouldMatch` "no-endpoint"

testPatchApps :: (HasCallStack) => App ()
testPatchApps = checkPatch OwnDomain "apps" disabled
