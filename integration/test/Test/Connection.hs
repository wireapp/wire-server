-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2023 Wire Swiss GmbH <opensource@wire.com>
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
module Test.Connection where

import API.Brig (getConnection, postConnection, putConnection)
import API.Galley
import SetupHelpers
import Testlib.Prelude
import UnliftIO.Async (forConcurrently_)

testConnectWithRemoteUser :: HasCallStack => Domain -> App ()
testConnectWithRemoteUser owningDomain = do
  (alice, bob, one2oneId) <- createOne2OneConversation owningDomain
  aliceId <- alice %. "qualified_id"
  getConversation alice one2oneId `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    others <- resp.json %. "members.others" & asList
    qIds <- for others (%. "qualified_id")
    qIds `shouldMatchSet` ([] :: [Value])
  void $ putConnection bob alice "accepted" >>= getBody 200
  getConversation bob one2oneId `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    others <- resp.json %. "members.others" & asList
    qIds <- for others (%. "qualified_id")
    qIds `shouldMatchSet` [aliceId]

testRemoteUserGetsDeleted :: App ()
testRemoteUserGetsDeleted = do
  alice <- randomUser OwnDomain def

  charlieConnected <- do
    charlie <- randomUser OtherDomain def
    connectTwoUsers alice charlie
    pure charlie

  charliePending <- do
    charlie <- randomUser OtherDomain def
    -- the connection should be pending here
    postConnection alice charlie `bindResponse` \resp ->
      resp.status `shouldMatchInt` 201

    getConnection alice charlie `bindResponse` \resp -> do
      resp.json %. "status" `shouldMatch` "sent"
      resp.status `shouldMatchInt` 200

    getConnection charlie alice `waitForResponse` \resp -> do
      resp.json %. "status" `shouldMatch` "pending"
      resp.status `shouldMatchInt` 200

    pure charlie

  charlieBlocked <- do
    charlie <- randomUser OtherDomain def
    postConnection alice charlie `bindResponse` \resp ->
      resp.status `shouldMatchInt` 201

    putConnection charlie alice "blocked" `bindResponse` \resp ->
      resp.status `shouldMatchInt` 200

    getConnection charlie alice `bindResponse` \resp -> do
      resp.json %. "status" `shouldMatch` "blocked"
      resp.status `shouldMatchInt` 200

    pure charlie

  charlieUnconnected <- do
    randomUser OtherDomain def

  forConcurrently_ [charliePending, charlieConnected, charlieBlocked, charlieUnconnected] \charlie -> do
    deleteUser charlie

    -- charlie is on their local backend, so asking should be instant
    getConnection charlie alice `bindResponse` \resp ->
      resp.status `shouldMatchInt` 404

    -- for alice, charlie is on the remote backend, so the status change
    -- may not be instant
    getConnection alice charlie `waitForResponse` \resp ->
      resp.status `shouldMatchInt` 404

assertConnectionStatus ::
  ( HasCallStack,
    MakesValue userFrom,
    MakesValue userTo
  ) =>
  userFrom ->
  userTo ->
  String ->
  App ()
assertConnectionStatus userFrom userTo connStatus =
  getConnection userFrom userTo `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "status" `shouldMatch` connStatus

testConnectFromIgnored :: HasCallStack => App ()
testConnectFromIgnored = do
  [alice, bob] <- forM [OwnDomain, OtherDomain] $ flip randomUser def
  void $ postConnection bob alice >>= getBody 201
  -- set up an initial "ignored" state on Alice's side
  assertConnectionStatus alice bob "pending"
  void $ putConnection alice bob "ignored" >>= getBody 200
  assertConnectionStatus alice bob "ignored"

  -- if Bob sends a new connection request, Alice goes back to "pending"
  void $ postConnection bob alice >>= getBody 200
  assertConnectionStatus alice bob "pending"

  -- if Alice accepts, and Bob still wants to connect, Alice transitions to
  -- "accepted"
  putConnection alice bob "accepted" `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "status" `shouldMatch` "accepted"
