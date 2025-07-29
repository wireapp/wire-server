module Test.FeatureFlags.ConsumableNotifications where

import qualified API.GalleyInternal as Internal
import SetupHelpers
import Test.FeatureFlags.Util
import Testlib.Prelude

testConsumableNotificationsInternal :: (HasCallStack) => App ()
testConsumableNotificationsInternal = do
  (alice, tid, _) <- createTeam OwnDomain 0
  Internal.setTeamFeatureLockStatus alice tid "consumableNotifications" "unlocked"
  withWebSocket alice $ \ws -> do
    setFlag InternalAPI ws tid "consumableNotifications" enabled
    setFlag InternalAPI ws tid "consumableNotifications" disabled
