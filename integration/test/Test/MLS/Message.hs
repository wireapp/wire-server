module Test.MLS.Message where

import API.Galley
import MLS.Util
import Notifications
import SetupHelpers
import Testlib.Prelude

testAppMessageSomeReachable :: HasCallStack => App ()
testAppMessageSomeReachable = do
  (alice1, charlie) <- startDynamicBackends [mempty] $ \[thirdDomain] -> do
    ownDomain <- make OwnDomain & asString
    otherDomain <- make OtherDomain & asString
    [alice, bob, charlie] <- createAndConnectUsers [ownDomain, otherDomain, thirdDomain]

    [alice1, bob1, charlie1] <- traverse (createMLSClient def) [alice, bob, charlie]
    traverse_ uploadNewKeyPackage [bob1, charlie1]
    void $ createNewGroup alice1
    void $ withWebSocket charlie $ \ws -> do
      void $ createAddCommit alice1 [bob, charlie] >>= sendAndConsumeCommitBundle
      awaitMatch 10 isMemberJoinNotif ws
    pure (alice1, charlie)

  mp <- createApplicationMessage alice1 "hi, bob!"
  bindResponse (postMLSMessage mp.sender mp.message) $ \resp -> do
    resp.status `shouldMatchInt` 201

    charlieId <- charlie %. "qualified_id"
    resp.json %. "failed_to_send" `shouldMatchSet` [charlieId]
