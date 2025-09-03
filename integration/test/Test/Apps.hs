{-# OPTIONS -Wno-ambiguous-fields #-}
module Test.Apps where

import API.Brig
import SetupHelpers
import Testlib.Prelude

testCreateApp :: (HasCallStack) => App ()
testCreateApp = do
  (alice, tid, _) <- createTeam OwnDomain 1
  bindResponse (createApp alice tid def {name = "chappie"}) $ \resp -> do
    resp.status `shouldMatchInt` 200
    printJSON resp.json
