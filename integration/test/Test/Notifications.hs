{-# OPTIONS -Wno-ambiguous-fields #-}

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

module Test.Notifications where

import API.Brig
import API.Common
import API.Gundeck
import API.GundeckInternal
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Notifications
import SetupHelpers
import Testlib.Prelude
import UnliftIO (forConcurrently_)

examplePush :: (MakesValue u) => u -> App Value
examplePush u = do
  r <- recipient u
  pure
    $ object
      [ "recipients" .= [r],
        "payload" .= [object ["hello" .= "world"]]
      ]

testFetchAllNotifications :: App ()
testFetchAllNotifications = do
  user <- randomUserId OwnDomain
  push <- examplePush user

  let n = 10
  replicateM_ n
    $ bindResponse (postPush user [push])
    $ \res ->
      res.status `shouldMatchInt` 200

  let c :: Maybe String = Just "deadbeef"
  ns <- getNotifications user (def {client = c} :: GetNotifications) >>= getJSON 200

  expected <- replicateM n (push %. "payload")
  allNotifs <- ns %. "notifications" & asList
  actual <- traverse (%. "payload") allNotifs
  actual `shouldMatch` expected

  firstNotif <-
    getNotification
      user
      (def {client = c} :: GetNotification)
      (head allNotifs %. "id")
      >>= getJSON 200
  firstNotif `shouldMatch` head allNotifs

  lastNotif <-
    getLastNotification
      user
      (def {client = c} :: GetNotification)
      >>= getJSON 200
  lastNotif `shouldMatch` last allNotifs

testLastNotification :: App ()
testLastNotification = do
  user <- randomUserId OwnDomain
  userId <- user %. "id" & asString
  let push c =
        object
          [ "recipients"
              .= [ object
                     [ "user_id" .= userId,
                       "route" .= "any",
                       "clients" .= [c]
                     ]
                 ],
            "payload" .= [object ["client" .= c]]
          ]

  for_ ["a", "b", "c", "d", "e", "f"] $ \c ->
    bindResponse (postPush user [push c]) $ \res ->
      res.status `shouldMatchInt` 200

  lastNotif <- getLastNotification user def {client = Just "c"} >>= getJSON 200
  lastNotif %. "payload" `shouldMatch` [object ["client" .= "c"]]

testInvalidNotification :: (HasCallStack) => App ()
testInvalidNotification = do
  user <- randomUserId OwnDomain

  -- test uuid v4 as "since"
  do
    notifId <- randomId
    void
      $ getNotifications user def {since = Just notifId}
      >>= getJSON 400

  -- test arbitrary uuid v1 as "since"
  do
    notifId <- randomUUIDv1
    void
      $ getNotifications user def {since = Just notifId}
      >>= getJSON 404

-- | Check that client-add notifications use the V6 format:
-- @
--   "capabilities": { "capabilities": [..] }
-- @
--
-- Migration plan: clients must be able to parse both old and new schema starting from V7.  Once V6 is deprecated, the backend can start sending notifications in the new form.
testAddClientNotification :: (HasCallStack) => App ()
testAddClientNotification = do
  alice <- randomUser OwnDomain def

  e <- withWebSocket alice $ \ws -> do
    void $ addClient alice def
    n <- awaitMatch isUserClientAddNotif ws
    nPayload n

  void $ e %. "client.capabilities.capabilities" & asList

testBulkPushSameMessage :: (HasCallStack) => App ()
testBulkPushSameMessage = do
  let numUsers = 20
      numClientsPerUser = 8
  quids <- replicateM numUsers $ randomUserId OwnDomain
  quidsAndClients <- for quids $ \quid -> (quid,) <$> replicateM numClientsPerUser randomClientId

  let mkIndividualPayload quid cid = object ["qualified_id" .= quid, "client" .= cid]
  individualPushes <- fmap mconcat $ for quidsAndClients $ \(quid, cids) -> do
    uid <- quid %. "id" & asString
    fmap mconcat . for cids $ \cid ->
      pure
        [ object
            [ "recipients"
                .= [ object
                       [ "user_id" .= uid,
                         "clients" .= [cid],
                         "route" .= "any"
                       ]
                   ],
              "payload" .= [mkIndividualPayload quid cid]
            ]
        ]

  uidsAndClients <- for quidsAndClients $ \(quid, cids) -> do
    uid <- quid %. "id" & asString
    pure $ (uid, cids)
  let groupPush =
        object
          [ "recipients"
              .= ( concat . for uidsAndClients $ \(uid, cids) ->
                     [object ["user_id" .= uid, "clients" .= cids, "route" .= "any"]]
                 ),
            "payload" .= [object ["message" .= "hello everyone"]]
          ]

  quidsConnsAndClients <- fmap mconcat . for quidsAndClients $ \(quid, cids) ->
    for cids $ \cid -> (quid,,cid) <$> randomConnId
  withWebSockets quidsConnsAndClients $ \websockets -> do
    pusher <- randomUserId OwnDomain
    postPush pusher individualPushes >>= assertSuccess
    forConcurrently_ websockets $ \ws -> do
      event <- assertJust "Expected an event, got Nothing" =<< awaitAnyEvent 1 ws
      event %. "payload.0.qualified_id.id" `shouldMatch` ws.wsConnect.user
      event %. "payload.0.qualified_id.domain" `shouldMatch` ws.wsConnect.domain
      event %. "payload.0.client" `shouldMatch` ws.wsConnect.client
      assertNoEvent 1 ws

    postPush pusher [groupPush] >>= assertSuccess
    forConcurrently_ websockets $ \ws -> do
      event <- assertJust "Expected an event, got Nothing" =<< awaitAnyEvent 1 ws
      event %. "payload.0.message" `shouldMatch` "hello everyone"
      assertNoEvent 1 ws

testGetServerTime :: (HasCallStack) => App ()
testGetServerTime = do
  user <- randomUser OwnDomain def
  formattedTimestampStr <-
    getServerTime user `bindResponse` \r -> do
      r.status `shouldMatchInt` 200
      r.json %. "time" & asString
  void $ assertJust ("expected ISO 8601 format, but got: " <> formattedTimestampStr) $ iso8601ParseM @Maybe @UTCTime formattedTimestampStr

testTargetClientPush :: (HasCallStack) => App ()
testTargetClientPush = do
  user <- randomUserId OwnDomain
  userId <- user %. "id" & asString
  cid1 <- randomClientId
  connId1 <- randomConnId
  cid2 <- randomClientId
  connId2 <- randomConnId
  let push c =
        object
          [ "recipients"
              .= [ object
                     [ "user_id" .= userId,
                       "route" .= "any",
                       "clients" .= [c]
                     ]
                 ],
            "payload" .= [object ["client" .= c]]
          ]
  withWebSockets [(user, connId1, cid1), (user, connId2, cid2)] $ \[ws1, ws2] -> do
    postPush user [push cid1, push cid2] >>= assertSuccess
    forConcurrently_ [(cid1, ws1), (cid2, ws2)] $ \(cid, ws) -> do
      ev <- awaitAnyEvent 2 ws
      ev %. "payload" `shouldMatch` [object ["client" .= cid]]
      assertNoEvent 2 ws
      -- Also check the notification stream
      lastNotif <- getLastNotification user def {client = Just cid} >>= getJSON 200
      lastNotif %. "payload" `shouldMatch` [object ["client" .= cid]]
