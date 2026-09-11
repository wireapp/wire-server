-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

module Test.NotificationsVersioned where

import API.Brig (addClient, putHandle)
import API.BrigCommon (AddClient (..))
import API.Common (randomHandle)
import API.Galley
import Control.Monad.Codensity (runCodensity)
import Data.Time.Clock
import Notifications (isConvCreateMeetingNotif, isMeetingCreateNotif)
import SetupHelpers
import Test.Events (ackEvent, assertFindsEvent, consumeAllEventsNoAck, createEventsWebSocketAtVersion, enableConsumableNotifications)
import Test.Meetings (defaultMeetingJson)
import Testlib.Cannon
import Testlib.Prelude
import UnliftIO.Concurrent (threadDelay)

gatedTypes :: [String]
gatedTypes =
  [ "conversation.create-meeting",
    "conversation.delete-meeting",
    "meeting.create",
    "meeting.update",
    "meeting.delete",
    "meeting.member-add"
  ]

isGated :: String -> Bool
isGated t = t `elem` gatedTypes

-- | Drain all notification pages at the given API version and return the
-- payload event types seen.  Also asserts that pagination terminates (no
-- endless empty 'has_more=true' pages).
drainNotificationsAt :: (HasCallStack, MakesValue user) => user -> Int -> App [String]
drainNotificationsAt user v = go Nothing []
  where
    go since acc = do
      req <- baseRequest user Gundeck (ExplicitVersion v) "/notifications"
      let req' =
            req
              & addQueryParams
                ( [("since", s) | s <- toList since]
                    <> [("size", "100")]
                )
      r <- submit "GET" req'
      r.status `shouldMatchInt` 200
      body <- getJSON 200 r
      notifications <- body %. "notifications" & asList
      types <-
        mconcat
          <$> for
            notifications
            ( \n -> do
                payload <- n %. "payload" & asList
                for payload (\e -> e %. "type" >>= asString)
            )
      lastId <- case reverse notifications of
        [] -> pure Nothing
        (n : _) -> Just <$> (n %. "id" >>= asString)
      hasMore <- body %. "has_more" & asBool
      if hasMore
        then case lastId of
          Just l -> go (Just l) (acc <> types)
          Nothing -> assertFailure "has_more=true but no notification id for cursor"
        else pure (acc <> types)

mkMeeting :: App Value
mkMeeting = do
  now <- liftIO getCurrentTime
  let startTime = addUTCTime 3600 now
      endTime = addUTCTime 7200 now
  pure $ defaultMeetingJson "Versioned meeting" startTime endTime []

-- | The meeting creator (who receives the meeting events) must not see them
-- via a V14 fetch, while a current-version fetch of the same window shows
-- them; V14 pagination terminates.
testVersionedNotificationsHideMeetingEvents :: (HasCallStack) => App ()
testVersionedNotificationsHideMeetingEvents = do
  (alice, _tid, _members) <- createTeam OwnDomain 1
  meeting <- mkMeeting

  withWebSocket alice $ \wsAlice -> do
    resp <- postMeetings alice meeting
    assertSuccess resp
    -- the current-version websocket sees the meeting events
    void $ awaitMatch isConvCreateMeetingNotif wsAlice
    void $ awaitMatch isMeetingCreateNotif wsAlice

  v14Types <- drainNotificationsAt alice 14
  filter isGated v14Types `shouldMatch` ([] :: [String])

  curTypes <- drainNotificationsAt alice 17
  curTypes `shouldContain` ["conversation.create-meeting"]
  curTypes `shouldContain` ["meeting.create"]

-- | A V14 client still sees non-meeting events (e.g. conversation.create)
-- while meeting events are filtered from the same window.
testVersionedNotificationsKeepNonMeetingEvents :: (HasCallStack) => App ()
testVersionedNotificationsKeepNonMeetingEvents = do
  (alice, tid, [bob]) <- createTeam OwnDomain 2
  resp <-
    postConversation
      alice
      defProteus
        { qualifiedUsers = [bob],
          name = Just "plain conv",
          team = Just tid
        }
  assertSuccess resp
  meeting <- mkMeeting
  mresp <- postMeetings alice meeting
  assertSuccess mresp

  -- bob sees the plain conversation event, but no meeting events (the
  -- meeting's gated events go to alice, and none leak to bob at V14).
  bobTypes <- drainNotificationsAt bob 14
  bobTypes `shouldContain` ["conversation.create"]
  filter isGated bobTypes `shouldMatch` ([] :: [String])

  aliceTypes <- drainNotificationsAt alice 14
  aliceTypes `shouldContain` ["conversation.create"]
  filter isGated aliceTypes `shouldMatch` ([] :: [String])

-- | A websocket connected at a low version receives no meeting event frames,
-- while a current-version connection of the same user does.
testVersionedWebSocketFiltersMeetingEvents :: (HasCallStack) => App ()
testVersionedWebSocketFiltersMeetingEvents = do
  (alice, _tid, _members) <- createTeam OwnDomain 1
  aliceId <- alice %. "id" >>= asString
  aliceDomain <- objDomain alice
  meeting <- mkMeeting

  let lowV = WSConnect aliceId aliceDomain Nothing (Just "lowconn") (Just 14)
      highV = WSConnect aliceId aliceDomain Nothing (Just "highconn") Nothing

  withWebSocket lowV $ \wsLow ->
    withWebSocket highV $ \wsHigh -> do
      resp <- postMeetings alice meeting
      assertSuccess resp
      -- current version gets the meeting events ...
      void $ awaitMatch isConvCreateMeetingNotif wsHigh
      void $ awaitMatch isMeetingCreateNotif wsHigh
      -- ... the low version does not (allow some time for delivery)
      liftIO $ threadDelay 1_000_000
      assertNoEvent 1 wsLow

-- | The rabbitmq-backed /events websocket of a low-version client skips (and
-- server-side acks) meeting event frames, while a current-version connection
-- of the same user receives them.  Tolerant drain: stray ungated events are
-- allowed on the low socket, gated ones are not.
testVersionedEventsSocketFiltersMeetingEvents :: (HasCallStack) => App ()
testVersionedEventsSocketFiltersMeetingEvents =
  withModifiedBackend (enableConsumableNotifications def) $ \domain -> do
    (alice, _tid, _members) <- createTeam domain 1
    -- mirror the other temp-/events tests in Test.Events: create a
    -- consumable-notifications client for alice
    void $ addClient alice def {acapabilities = Just ["consumable-notifications"]} >>= getJSON 201
    -- Two temp queues with no client id (each binds userRoutingKey and gets its
    -- own version-filtered consumer); sharing a client id would round-robin a
    -- single queue and race.
    runCodensity (createEventsWebSocketAtVersion alice Nothing 14) $ \wsLow ->
      runCodensity (createEventsWebSocketAtVersion alice Nothing 17) $ \wsHigh -> do
        meeting <- mkMeeting
        postMeetings alice meeting >>= assertSuccess
        assertFindsEvent wsHigh $ \e -> do
          e %. "type" `shouldMatch` "event"
          t <- e %. "data.event.payload.0.type" >>= asString
          unless (isGated t)
            $ assertFailure ("expected a gated meeting event on the V17 socket, got: " <> t)
          ackEvent wsHigh e
        -- allow some time for delivery, then tolerate stray ungated events on
        -- the low socket but assert that none of them is gated
        liftIO $ threadDelay 1_000_000
        drained <- consumeAllEventsNoAck wsLow
        types <- traverse (\e -> e %. "data.event.payload.0.type" >>= asString) drained
        filter isGated types `shouldMatch` ([] :: [String])

-- | A V14 notification cursor is not stranded on an all-gated page: the
-- gundeck refill loop must skip past a fully-gated page (server minimum page
-- size is 100) and still deliver a later ungated event.
testVersionedNotificationsRefillPastGatedBacklog :: (HasCallStack) => App ()
testVersionedNotificationsRefillPastGatedBacklog = do
  (alice, _tid, _members) <- createTeam OwnDomain 1
  -- 101 meetings (~30-60 s by design) guarantee > 100 gated notification rows
  -- for the creator even if galley batches conversation.create-meeting and
  -- meeting.create into a single row.
  replicateM_ 101 $ do
    meeting <- mkMeeting
    postMeetings alice meeting >>= assertSuccess
  -- The ungated event must land strictly after the gated backlog (gundeck
  -- persists notifications synchronously in the request path).  If that ever
  -- becomes async, user.update could land inside the first 100 rows and this
  -- test would silently degrade to never exercising the refill loop.
  handle <- randomHandle
  putHandle alice handle >>= assertSuccess

  v14 <- drainNotificationsAt alice 14
  v14 `shouldContain` ["user.update"]
  filter isGated v14 `shouldMatch` ([] :: [String])

  v17 <- drainNotificationsAt alice 17
  v17 `shouldContain` ["meeting.create"]
