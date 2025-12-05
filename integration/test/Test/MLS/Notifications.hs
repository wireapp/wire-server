-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Test.MLS.Notifications where

import API.Galley
import API.Gundeck
import Control.Concurrent
import Data.Timeout
import MLS.Util
import Notifications
import SetupHelpers
import Testlib.Prelude

testWelcomeNotification :: (HasCallStack) => App ()
testWelcomeNotification = do
  [alice, bob] <- createAndConnectUsers [OwnDomain, OtherDomain]
  [alice1, alice2, bob1, bob2] <- traverse (createMLSClient def) [alice, alice, bob, bob]
  traverse_ (uploadNewKeyPackage def) [alice2, bob1, bob2]

  convId <- createNewGroup def alice1
  notif <- withWebSocket bob $ \ws -> do
    void $ createAddCommit alice1 convId [alice, bob] >>= sendAndConsumeCommitBundle
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

testNotificationPagination :: (HasCallStack) => App ()
testNotificationPagination = do
  {-
  Create a user with client1

  Wait for all the notifications to expire (there should be no notifications which are sent to “All Clients”, i.e. there should be no notifications in the DB where clients field is null.

  Send a lot of MLS messages to this client (so that the total payload size goes over 5 MB)

  Create another client (say client2)

  Fetch notifications for client 2
  -}

  let overrides = def {gundeckCfg = setField "settings.notificationTTL" (2 #> Second)}
  withModifiedBackend overrides $ \dom -> do
    -- create a user with client1
    [alice, bob] <- createAndConnectUsers [dom, dom]
    [alice1, bob1] <- traverse (createMLSClient def) [alice, bob]
    traverse_ (uploadNewKeyPackage def) [alice1, bob1]

    conv <- postConversation alice1 defMLS >>= getJSON 201
    _convQid <- conv %. "qualified_id"
    convId <- objConvId conv
    createGroup def alice1 convId
    void $ createAddCommit alice1 convId [bob] >>= sendAndConsumeCommitBundle

    -- let notifs expire
    liftIO $ threadDelay 2_300_000

    -- bob sends alice > 5MB of MLS messages
    let payload = replicate 20_000 '.'
    forM_ [1 :: Int .. 260] $ \i -> do
      print i
      void $ createApplicationMessage convId bob1 payload >>= sendAndConsumeMessage

    -- create new client for alice, fetch notifications, and check response consistency.
    alice2 :: ClientIdentity <- createMLSClient def alice

    getNotifications
      alice
      def {since = Nothing, size = Nothing, client = Just alice2.client}
      `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        resp.json %. "has_more" `shouldMatch` False
        nfs :: [Value] <- resp.json %. "notifications" >>= asList
        length nfs `shouldMatchInt` 1
