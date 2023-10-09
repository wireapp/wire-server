module Test.MLS.Message where

import MLS.Util
import Notifications
import SetupHelpers
import Testlib.Prelude

testAppMessageSomeReachable :: HasCallStack => App ()
testAppMessageSomeReachable = do
  alice1 <- startDynamicBackends [mempty] $ \[thirdDomain] -> do
    ownDomain <- make OwnDomain & asString
    otherDomain <- make OtherDomain & asString
    [alice, bob, charlie] <- createAndConnectUsers [ownDomain, otherDomain, thirdDomain]

    [alice1, bob1, charlie1] <- traverse (createMLSClient def) [alice, bob, charlie]
    traverse_ uploadNewKeyPackage [bob1, charlie1]
    void $ createNewGroup alice1
    void $ withWebSocket charlie $ \ws -> do
      void $ createAddCommit alice1 [bob, charlie] >>= sendAndConsumeCommitBundle
      awaitMatch 10 isMemberJoinNotif ws
    pure alice1

  void $ createApplicationMessage alice1 "hi, bob!" >>= sendAndConsumeMessage
