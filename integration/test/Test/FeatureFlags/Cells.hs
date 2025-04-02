module Test.FeatureFlags.Cells where

import qualified API.GalleyInternal as Internal
import SetupHelpers
import Test.FeatureFlags.Util
import Testlib.Prelude

testPatchCells :: (HasCallStack) => App ()
testPatchCells = checkPatch OwnDomain "cells" enabled

testCellsInternal :: (HasCallStack) => App ()
testCellsInternal = do
  (alice, tid, _) <- createTeam OwnDomain 0
  Internal.setTeamFeatureLockStatus alice tid "cells" "unlocked"
  withWebSocket alice $ \ws -> do
    setFlag InternalAPI ws tid "cells" enabled
    setFlag InternalAPI ws tid "cells" disabled
