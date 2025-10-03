module Test.FeatureFlags.StealthUsers where

import qualified API.GalleyInternal as Internal
import SetupHelpers
import Test.FeatureFlags.Util
import Testlib.Prelude

testStealthUsersInternal :: (HasCallStack) => App ()
testStealthUsersInternal = do
  (alice, tid, _) <- createTeam OwnDomain 0
  Internal.setTeamFeatureLockStatus alice tid "stealthUsers" "unlocked"
  withWebSocket alice $ \ws -> do
    setFlag InternalAPI ws tid "stealthUsers" enabled
    setFlag InternalAPI ws tid "stealthUsers" disabled
  Internal.setTeamFeatureLockStatus alice tid "stealthUsers" "locked"
  setFeature InternalAPI alice tid "stealthUsers" enabled `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 409
    resp.json %. "label" `shouldMatch` "feature-locked"
  -- the feature does not have a public PUT endpoint
  setFeature PublicAPI alice tid "stealthUsers" enabled `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 404
    resp.json %. "label" `shouldMatch` "no-endpoint"

testPatchStealthUsers :: (HasCallStack) => App ()
testPatchStealthUsers = checkPatch OwnDomain "stealthUsers" disabled
