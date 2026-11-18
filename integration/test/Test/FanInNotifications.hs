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

import API.GundeckInternal (postFanInPush)
import Data.Bits
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import SetupHelpers
import Testlib.PG (queryFanInSql)
import Testlib.Prelude

-- TEMPORARY: these SQL assertions depart from the black-box integration-test
-- convention because fan-in delivery is not implemented yet. Once the full
-- publish -> WebSocket -> acknowledgement -> reconnect round trip is available,
-- retain the behavioral scenarios but replace database inspection with client
-- observations, and remove queryFanInSql. The disabled-endpoint test below is
-- already black-box and can remain as it is.
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

testFanInDisabled :: (HasCallStack) => App ()
testFanInDisabled = withModifiedBackend def {gundeckCfg = setField "notificationFanInPostgres" Null} $ \domain -> do
  user <- randomUser domain def
  postFanInPush user [] >>= \response -> response.status `shouldMatchInt` 503

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
