{-# LANGUAGE OverloadedStrings #-}

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

module Test.FanInNotifications where

import API.Brig (addClient)
import API.Cannon
import API.GundeckInternal (postFanInPush, putFanInMembership)
import Data.Bits
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Network.WebSockets as WS
import SetupHelpers
import Testlib.PG (queryFanInSql)
import Testlib.Prelude
import UnliftIO (try)
import UnliftIO.Async (concurrently_)

-- TEMPORARY: these SQL assertions depart from the black-box integration-test
-- convention because fan-in delivery is not implemented yet. Once the full
-- publish -> WebSocket -> acknowledgement -> reconnect round trip is available,
-- retain the behavioral scenarios but replace database inspection with client
-- observations, and remove queryFanInSql.
testFanInPersistence :: (HasCallStack) => App ()
testFanInPersistence = do
  user <- randomUser OwnDomain def
  uid <- user %. "id" & asString
  domain <- asString OwnDomain
  other <- randomId
  teamId <- randomId
  groupId <- Text.unpack . decodeUtf8 . Base64.encode . UUID.toASCIIBytes <$> liftIO UUID.nextRandom
  newer <- notificationIdNow 0
  older <- notificationIdNow (-60)
  let userTarget = target "FanInTargetUser" (toJSON uid)
      targets = [userTarget, target "FanInTargetUserClients" (toJSON [toJSON uid, toJSON ["1" :: String, "2"]]), target "FanInTargetTeam" (toJSON teamId), target "FanInTargetEpoch" (toJSON [toJSON groupId, toJSON (7 :: Int)]), target "FanInTargetConnections" (object ["id" .= uid, "domain" .= domain]), target "FanInTargetConnections" (object ["id" .= uid, "domain" .= ("remote.example.com" :: String)])]
      request = push newer targets False
  postFanInPush user [request] >>= assertSuccess
  postFanInPush user [request] >>= assertSuccess
  postFanInPush user [push older [userTarget] False] >>= assertSuccess
  state <-
    queryFanInSql
      "SELECT json_build_object('users', (SELECT count(*) FROM user_notifications WHERE user_id = :'uid'::uuid), 'clients', (SELECT count(*) FROM client_notifications WHERE user_id = :'uid'::uuid), 'teams', (SELECT count(*) FROM team_notifications WHERE team_id = :'team'::uuid), 'epochs', (SELECT count(*) FROM epoch_notifications WHERE group_id = decode(:'group', 'base64') AND epoch = 7), 'local', (SELECT count(*) FROM local_connection_notifications WHERE user_id = :'uid'::uuid), 'remote', (SELECT count(*) FROM remote_connection_notifications WHERE user_id = :'uid'::uuid AND user_domain = 'remote.example.com'), 'isolated', (SELECT count(*) FROM user_notifications WHERE user_id = :'other'::uuid), 'head', (SELECT notification_id::text FROM last_user_notifications WHERE user_id = :'uid'::uuid), 'payload', (SELECT payload FROM user_notifications WHERE user_id = :'uid'::uuid AND notification_id = :'nid'::uuid))"
      [("uid", uid), ("other", other), ("team", teamId), ("group", groupId), ("nid", newer)]
  state `shouldMatch` object ["users" .= (2 :: Int), "clients" .= (2 :: Int), "teams" .= (1 :: Int), "epochs" .= (1 :: Int), "local" .= (1 :: Int), "remote" .= (1 :: Int), "isolated" .= (0 :: Int), "head" .= newer, "payload" .= [object ["type" .= ("fan-in.test" :: String)]]]

testFanInRejectsInvalidBatches :: (HasCallStack) => App ()
testFanInRejectsInvalidBatches = do
  user <- randomUser OwnDomain def
  uid <- user %. "id" & asString
  valid <- notificationIdNow 0
  invalid <- randomId
  let targets = [target "FanInTargetUser" (toJSON uid)]
  postFanInPush user [push valid targets False, push invalid targets False] >>= \response -> response.status `shouldMatchInt` 400
  postFanInPush user [push valid targets False, push valid targets True] >>= \response -> response.status `shouldMatchInt` 501
  state <- queryFanInSql "SELECT to_json(count(*)) FROM user_notifications WHERE user_id = :'uid'::uuid" [("uid", uid)]
  state `shouldMatchInt` 0

-- These history assertions are temporary for the same reason as the persistence
-- test above. Replace them with retained-epoch delivery checks in the round trip.
testFanInMembership :: (HasCallStack) => App ()
testFanInMembership = do
  user <- randomUser OwnDomain def
  uid <- user %. "id" & asString
  groupId <- Text.unpack . decodeUtf8 . Base64.encode . UUID.toASCIIBytes <$> liftIO UUID.nextRandom
  let epochMember cid = object ["userId" .= uid, "clientId" .= (cid :: String)]
      update epoch members = object ["groupId" .= groupId, "epoch" .= (epoch :: Integer), "members" .= members]
      put epoch members = putFanInMembership user (update epoch members)
      check expectedEpoch current history = do
        state <-
          queryFanInSql
            "SELECT json_build_object('epoch', (SELECT epoch FROM epoch_heads WHERE group_id = decode(:'group', 'base64')), 'current', (SELECT count(*) FROM epoch_members WHERE group_id = decode(:'group', 'base64')), 'history', (SELECT count(*) FROM epoch_history WHERE group_id = decode(:'group', 'base64')))"
            [("group", groupId)]
        state `shouldMatch` object ["epoch" .= (expectedEpoch :: Integer), "current" .= (current :: Int), "history" .= (history :: Int)]
  -- Concurrent first writers must serialize and behave as identical retries.
  concurrently_ (put 7 [epochMember "1", epochMember "2"] >>= assertSuccess) (put 7 [epochMember "2", epochMember "1", epochMember "1"] >>= assertSuccess)
  check 7 2 0
  put 7 [epochMember "3"] >>= \response -> response.status `shouldMatchInt` 409
  put 6 [epochMember "3"] >>= \response -> response.status `shouldMatchInt` 409
  check 7 2 0
  put 8 [epochMember "2"] >>= assertSuccess
  put 8 [epochMember "2"] >>= assertSuccess
  check 8 1 2
  -- Empty memberships retain the epoch, including after retirement. An old
  -- retry cannot bring members back, nor can a conflicting retry at that epoch.
  put 9 ([] :: [Value]) >>= assertSuccess
  put 9 ([] :: [Value]) >>= assertSuccess
  put 8 [epochMember "2"] >>= \response -> response.status `shouldMatchInt` 409
  put 9 [epochMember "2"] >>= \response -> response.status `shouldMatchInt` 409
  check 9 0 3
  put 10 [epochMember "3"] >>= assertSuccess
  check 10 1 3

-- A group first observed with no members still needs a durable epoch. Epochs
-- use the full MLS Word64 range even though PostgreSQL stores signed bigint.
testFanInEmptyMembership :: (HasCallStack) => App ()
testFanInEmptyMembership = do
  user <- randomUser OwnDomain def
  groupId <- Text.unpack . decodeUtf8 . Base64.encode . UUID.toASCIIBytes <$> liftIO UUID.nextRandom
  let put epoch = putFanInMembership user (object ["groupId" .= groupId, "epoch" .= (epoch :: Integer), "members" .= ([] :: [Value])])
  put 0 >>= assertSuccess
  put (2 ^ (63 :: Int)) >>= assertSuccess
  put (2 ^ (63 :: Int) - 1) >>= \response -> response.status `shouldMatchInt` 409
  put (2 ^ (64 :: Int) - 1) >>= assertSuccess
  put (2 ^ (64 :: Int) - 1) >>= assertSuccess

-- These assertions observe the public WebSocket, with no SQL access. Explicit
-- wake-ups stand in for the background publishing job, which is a later slice.
testFanInWebSocketRoundTrip :: (HasCallStack) => App ()
testFanInWebSocketRoundTrip = do
  user <- randomUser OwnDomain def
  uid <- objId user
  client <- addClient user def >>= getJSON 201
  cid <- objId client
  first <- notificationIdNow 0
  second <- notificationIdNow 1
  let targets = [target "FanInTargetUser" (toJSON uid)]
      publish nid = do
        let request = push nid targets False
        postFanInPush user [request] >>= assertSuccess
        notifyFanInCannon user (object ["recipients" .= [object ["userId" .= uid, "clientId" .= cid]], "push" .= request]) >>= assertSuccess
      receiveEvent socket expectedId = do
        message <- receiveFanInMessage socket
        message %. "type" `shouldMatch` ("event" :: String)
        message %. "data.event.id" `shouldMatch` expectedId
        message %. "data.event.payload.0.type" `shouldMatch` ("fan-in.test" :: String)
        message %. "data.delivery_tag"
      ack socket tag = sendFanInMessage socket (object ["type" .= ("ack" :: String), "delivery_tag" .= tag, "multiple" .= False])
  -- The first connection establishes an empty-stream boundary. Leave its first delivery
  -- unacknowledged so a fresh socket must replay it.
  withFanInWebSocket user (Just cid) $ \socket -> do
    receiveFanInMessage socket >>= (%. "type") >>= (`shouldMatch` ("synchronization" :: String))
    publish first
    _ <- receiveEvent socket first
    pure ()
  withFanInWebSocket user (Just cid) $ \socket -> do
    tag <- receiveEvent socket first
    receiveFanInMessage socket >>= (%. "type") >>= (`shouldMatch` ("synchronization" :: String))
    ack socket tag
    -- A later delivery proves Cannon processed the preceding ack before this
    -- socket closes. It also leaves another outstanding event for reconnect.
    publish second
    _ <- receiveEvent socket second
    pure ()
  withFanInWebSocket user (Just cid) $ \socket -> do
    tag <- receiveEvent socket second
    receiveFanInMessage socket >>= (%. "type") >>= (`shouldMatch` ("synchronization" :: String))
    ack socket tag
    -- Sending a duplicate ack is harmless; the next wake is processed after it.
    ack socket tag
    notifyFanInCannon user (object ["recipients" .= [object ["userId" .= uid, "clientId" .= cid]], "push" .= push second targets False]) >>= assertSuccess

-- Checking client ownership happens before accepting the upgraded connection.
testFanInWebSocketRejectsForeignClient :: (HasCallStack) => App ()
testFanInWebSocketRejectsForeignClient = do
  user <- randomUser OwnDomain def
  other <- randomUser OwnDomain def
  foreignClient <- addClient other def >>= getJSON 201 >>= objId
  result <- try @App @WS.HandshakeException (withFanInWebSocket user (Just foreignClient) (const (pure ())))
  case result of
    Left (WS.MalformedResponse response _) -> response.responseCode `shouldMatchInt` 403
    Left err -> assertFailure ("Expected client-ownership rejection with status 403, got " <> displayException err)
    Right () -> assertFailure "A user opened a fan-in stream using another user's client"

target :: String -> Value -> Value
target tag contents = object ["tag" .= tag, "contents" .= contents]

push :: String -> [Value] -> Bool -> Value
push nid targets transient =
  object
    ["notificationId" .= nid, "targets" .= targets, "conn" .= Null, "transient" .= transient, "route" .= ("any" :: String), "nativePriority" .= Null, "origin" .= Null, "json" .= object ["type" .= ("fan-in.test" :: String)], "apsData" .= Null, "isCellsEvent" .= False]

-- Use fresh UUIDv7 times so the rows land in existing Partman partitions.
notificationIdNow :: Integer -> App String
notificationIdNow offset = liftIO $ do
  now <- getPOSIXTime
  random <- UUID.nextRandom
  let milliseconds = floor (now * 1000) + offset * 1000
      (_, _, r1, r2) = UUID.toWords random
  pure $ UUID.toString (UUID.fromWords (fromIntegral (milliseconds `shiftR` 16)) ((fromIntegral milliseconds `shiftL` 16) .|. 0x7000) ((r1 .&. 0x3fffffff) .|. 0x80000000) r2)
