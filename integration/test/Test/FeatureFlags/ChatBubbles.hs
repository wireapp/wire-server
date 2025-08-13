module Test.FeatureFlags.ChatBubbles where

import qualified API.GalleyInternal as Internal
import SetupHelpers
import Test.FeatureFlags.Util
import Testlib.Prelude

testChatBubblesInternal :: (HasCallStack) => App ()
testChatBubblesInternal = do
  (alice, tid, _) <- createTeam OwnDomain 0
  Internal.setTeamFeatureLockStatus alice tid "chatBubbles" "unlocked"
  withWebSocket alice $ \ws -> do
    setFlag InternalAPI ws tid "chatBubbles" enabled
    setFlag InternalAPI ws tid "chatBubbles" disabled
  Internal.setTeamFeatureLockStatus alice tid "chatBubbles" "locked"
  setFeature InternalAPI alice tid "chatBubbles" enabled `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 409
    resp.json %. "label" `shouldMatch` "feature-locked"
  -- the feature does not have a public PUT endpoint
  setFeature PublicAPI alice tid "chatBubbles" enabled `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 404
    resp.json %. "label" `shouldMatch` "no-endpoint"

testPatchChatBubbles :: (HasCallStack) => App ()
testPatchChatBubbles = checkPatch OwnDomain "chatBubbles" disabled
