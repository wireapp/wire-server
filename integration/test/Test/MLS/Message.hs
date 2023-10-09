module Test.MLS.Message where

import MLS.Util
import Notifications
import SetupHelpers
import Testlib.Prelude

-- | Test happy case of federated MLS message sending in both directions.
testApplicationMessage :: HasCallStack => App ()
testApplicationMessage = do
  -- local alice and alex, remote bob
  [alice, alex, bob, betty] <-
    createUsers
      [OwnDomain, OwnDomain, OtherDomain, OtherDomain]
  for_ [alex, bob, betty] $ \user -> connectTwoUsers alice user

  clients@[alice1, _alice2, alex1, _alex2, bob1, _bob2, _, _] <-
    traverse
      (createMLSClient def)
      [alice, alice, alex, alex, bob, bob, betty, betty]
  traverse_ uploadNewKeyPackage clients
  void $ createNewGroup alice1

  withWebSockets [alice, alex, bob, betty] $ \wss -> do
    -- alice adds all other users (including her own client)
    void $ createAddCommit alice1 [alice, alex, bob, betty] >>= sendAndConsumeCommitBundle
    traverse_ (awaitMatch 10 isMemberJoinNotif) wss

    -- alex sends a message
    void $ createApplicationMessage alex1 "hello" >>= sendAndConsumeMessage
    traverse_ (awaitMatch 10 isNewMLSMessageNotif) wss

    -- bob sends a message
    void $ createApplicationMessage bob1 "hey" >>= sendAndConsumeMessage
    traverse_ (awaitMatch 10 isNewMLSMessageNotif) wss

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
