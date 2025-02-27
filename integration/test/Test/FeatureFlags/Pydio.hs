module Test.FeatureFlags.Pydio where

import qualified API.GalleyInternal as Internal
import SetupHelpers
import Test.FeatureFlags.Util
import Testlib.Prelude

testPatchPydio :: (HasCallStack) => App ()
testPatchPydio = checkPatch OwnDomain "pydio" enabled

testPydioInternal :: (HasCallStack) => App ()
testPydioInternal = do
  (alice, tid, _) <- createTeam OwnDomain 0
  Internal.setTeamFeatureLockStatus alice tid "pydio" "unlocked"
  withWebSocket alice $ \ws -> do
    setFlag InternalAPI ws tid "pydio" enabled
    setFlag InternalAPI ws tid "pydio" disabled
