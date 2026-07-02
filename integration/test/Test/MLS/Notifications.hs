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
import MLS.Util
import Notifications
import SetupHelpers
import Testlib.Prelude

-- | Return the id of the user's newest notification, if any.
--
-- Useful as a pagination anchor ('since'): instead of relying on a short
-- 'notificationTTL' to expire pre-existing notifications (e.g. the welcome
-- notification) before querying, we page from the newest existing one.
--
-- The welcome notification is pushed asynchronously on user creation, so it may
-- not have landed yet when we first look. We therefore poll until the set of
-- pre-existing notifications stabilizes (two consecutive reads agree) before
-- anchoring on the newest id. This restores the \"wait for the welcome
-- notification\" duty of the old fixed sleep without the TTL-expiry race: the
-- previous approach set a 2s TTL and slept for 2.1s, racing the anchor to
-- expiry before the final 'since' query (gundeck returns 404 by design when
-- the 'since' cursor is gone).
notificationAnchor :: (HasCallStack, MakesValue user) => user -> App (Maybe String)
notificationAnchor user = stabilize stabilizeRounds []
  where
    -- Total budget ~4s; we return early once the pre-existing notifications
    -- have settled, so the common case is fast.
    stabilizeRounds :: Int
    stabilizeRounds = 20

    stabilizeStepUs :: Int
    stabilizeStepUs = 200_000

    readIds :: App [String]
    readIds =
      getNotifications user def `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        ns <- resp.json %. "notifications" >>= asList
        -- gundeck returns notifications oldest-first, so the newest id is last.
        mapM (\n -> n %. "id" >>= asString) ns

    stabilize :: Int -> [String] -> App (Maybe String)
    stabilize 0 acc = pure (lastId acc)
    stabilize n prev = do
      cur <- readIds
      if cur == prev && not (null cur)
        then pure (lastId cur)
        else do
          liftIO $ threadDelay stabilizeStepUs
          stabilize (n - 1) cur

    lastId :: [String] -> Maybe String
    lastId [] = Nothing
    lastId xs = Just (last xs)

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
          }
  withModifiedBackend overrides $ \dom -> do
    user <- randomUser dom def

    -- Anchor on the newest pre-existing notification (e.g. the welcome
    -- notification) so it can be ignored via pagination instead of relying on
    -- TTL expiry.
    anchor <- notificationAnchor user

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
      getNotifications user def {since = anchor} `bindResponse` \resp -> do
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
          }
  withModifiedBackend overrides $ \dom -> do
    user <- randomUser dom def

    -- Anchor on the newest pre-existing notification (e.g. the welcome
    -- notification) so it can be ignored via pagination instead of relying on
    -- TTL expiry.
    anchor <- notificationAnchor user

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
      getNotifications user def {since = anchor} `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        notif <- resp.json %. "notifications" >>= asList >>= assertOne
        notif %. "id" >>= asString

    -- Send a second, small notification that should show up after the anchor.
    postPush user [mkPush smallPayload] >>= assertSuccess

    getNotifications user def {since = Just bigNotifId}
      `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        resp.json %. "has_more" `shouldMatch` False
        n <- resp.json %. "notifications" >>= asList >>= assertOne
        n %. "payload.0.blob" `shouldMatch` "ok"
