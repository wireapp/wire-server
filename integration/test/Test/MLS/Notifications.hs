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

import API.Common (recipient)
import API.Gundeck
import API.GundeckInternal (postPush)
import Control.Concurrent (threadDelay)
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
  let overrides =
        def
          { gundeckCfg =
              setField "settings.maxPayloadLoadSize" (Just ((2 :: Int) * 1024))
                >=> setField "settings.notificationTTL" (2 #> Second)
          }
  withModifiedBackend overrides $ \dom -> do
    user <- randomUser dom def

    liftIO $ threadDelay 2_100_000 -- let notifications expire

    -- Create a single oversized notification so Cassandra paging stops after the first row.
    r <- recipient user
    let bigPayload = replicate (3 * 1024) 'x' -- 3 KiB > maxPayloadLoadSize
        push =
          object
            [ "recipients" .= [r],
              "payload" .= [object ["blob" .= bigPayload]]
            ]

    postPush user [push] >>= assertSuccess

    notifId <-
      getNotifications user def `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        notif <- resp.json %. "notifications" >>= asList >>= assertOne
        notif %. "id" >>= asString

    -- Re-request starting after that notification
    getNotifications user def {since = Just notifId}
      `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        resp.json %. "notifications" >>= asList >>= shouldBeEmpty
        resp.json %. "has_more" `shouldMatch` False

testNotificationPaginationOversizeSince :: (HasCallStack) => App ()
testNotificationPaginationOversizeSince = do
  let overrides =
        def
          { gundeckCfg =
              setField "settings.maxPayloadLoadSize" (Just ((2 :: Int) * 1024))
                >=> setField "settings.notificationTTL" (2 #> Second)
          }
  withModifiedBackend overrides $ \dom -> do
    user <- randomUser dom def
    liftIO $ threadDelay 2_100_000 -- let notifications expire
    r <- recipient user
    let bigPayload = replicate (3 * 1024) 'x'
        smallPayload = "ok"
        mkPush payload =
          object
            [ "recipients" .= [r],
              "payload" .= [object ["blob" .= payload]]
            ]

    postPush user [mkPush bigPayload] >>= assertSuccess

    bigNotifId <-
      getNotifications user def `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        notif <- resp.json %. "notifications" >>= asList >>= assertOne
        notif %. "id" >>= asString

    -- Send a second, small notification that should show up after the anchor.
    postPush user [mkPush smallPayload] >>= assertSuccess

    getNotifications user def {since = Just bigNotifId}
      `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        notif <- resp.json %. "notifications" >>= asList >>= assertOne
        notif %. "payload[0].blob" `shouldMatch` "ok"
        resp.json %. "has_more" `shouldMatch` False
