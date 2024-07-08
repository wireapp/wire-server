module Test.MLS.Notifications where

import API.Gundeck
import MLS.Util
import Notifications
import SetupHelpers
import Testlib.Prelude

testWelcomeNotification :: (HasCallStack) => App ()
testWelcomeNotification = do
  [alice, bob] <- createAndConnectUsers [OwnDomain, OtherDomain]
  [alice1, alice2, bob1, bob2] <- traverse (createMLSClient def) [alice, alice, bob, bob]
  traverse_ uploadNewKeyPackage [alice2, bob1, bob2]

  void $ createNewGroup alice1
  notif <- withWebSocket bob $ \ws -> do
    void $ createAddCommit alice1 [alice, bob] >>= sendAndConsumeCommitBundle
    awaitMatch isWelcomeNotif ws

  notifId <- notif %. "id" & asString

  for_ [bob1, bob2] $ \cid ->
    getNotifications
      bob
      def
        { since = Just notifId,
          client = Just cid.client,
          size = Just 10000
        }
      >>= getJSON 200
