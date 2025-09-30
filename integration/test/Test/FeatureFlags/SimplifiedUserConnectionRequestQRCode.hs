module Test.FeatureFlags.SimplifiedUserConnectionRequestQRCode where

import qualified API.GalleyInternal as Internal
import SetupHelpers
import Test.FeatureFlags.Util
import Testlib.Prelude

testConnectionQRCodeInternal :: (HasCallStack) => App ()
testConnectionQRCodeInternal = do
  (alice, tid, _) <- createTeam OwnDomain 0
  Internal.setTeamFeatureLockStatus alice tid "simplifiedUserConnectionRequestQRCode" "unlocked"
  withWebSocket alice $ \ws -> do
    setFlag InternalAPI ws tid "simplifiedUserConnectionRequestQRCode" enabled
    setFlag InternalAPI ws tid "simplifiedUserConnectionRequestQRCode" disabled
  Internal.setTeamFeatureLockStatus alice tid "simplifiedUserConnectionRequestQRCode" "locked"
  setFeature InternalAPI alice tid "simplifiedUserConnectionRequestQRCode" enabled `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 409
    resp.json %. "label" `shouldMatch` "feature-locked"
  -- the feature does not have a public PUT endpoint
  setFeature PublicAPI alice tid "simplifiedUserConnectionRequestQRCode" enabled `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 404
    resp.json %. "label" `shouldMatch` "no-endpoint"
