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
import API.BrigInternal
import API.Galley
import Notifications
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

testRemoteUserGetsDeleted :: HasCallStack => App ()
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

testInternalGetConStatusesAll :: HasCallStack => App ()
testInternalGetConStatusesAll =
  startDynamicBackends [mempty] \[dynBackend] -> do
    let mkFiveUsers dom = replicateM 5 do
          randomUser dom def
    alices <- mkFiveUsers OwnDomain
    bobs <- mkFiveUsers OwnDomain
    charlies <- mkFiveUsers OtherDomain
    dylans <- mkFiveUsers dynBackend
    for_ alices \alicei -> do
      let connectWith users = do
            for_ users \useri ->
              postConnection alicei useri `bindResponse` \resp ->
                resp.status `shouldMatchInt` 201
            putConnection (head users) alicei "accepted" `bindResponse` \resp ->
              resp.status `shouldMatchOneOf` [Number 200, Number 204]
      -- local: connect each alice, accept only one
      connectWith bobs
      -- remote 1 & 2: connect each alice, accept only one
      connectWith charlies
      connectWith dylans

    getConnStatusForUsers alices OwnDomain `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 200
      conns <- asList resp.json
      let statusIs f =
            filterM
              do
                \conn -> do
                  s <- conn %. "status" & asString
                  pure $ f s
              conns

      sent <- statusIs (== "sent")
      accepted <- statusIs (== "accepted")
      other <- statusIs \v -> v /= "sent" && v /= "accepted"

      length other `shouldMatchInt` 0
      length accepted `shouldMatchInt` 15
      length sent `shouldMatchInt` 60

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

testSentFromIgnored :: HasCallStack => App ()
testSentFromIgnored = do
  [alice, bob] <- forM [OwnDomain, OtherDomain] $ flip randomUser def
  -- set up an initial "ignored" state
  void $ postConnection bob alice >>= getBody 201
  void $ putConnection alice bob "ignored" >>= getBody 200
  assertConnectionStatus alice bob "ignored"

  -- if Bob rescinds, Alice stays in "ignored"
  void $ putConnection bob alice "cancelled" >>= getBody 200
  assertConnectionStatus alice bob "ignored"

  -- if Alice accepts, and Bob does not want to connect anymore, Alice
  -- transitions to "sent"
  void $ putConnection alice bob "accepted" >>= getBody 200
  assertConnectionStatus alice bob "sent"

testConnectFromBlocked :: HasCallStack => App ()
testConnectFromBlocked = do
  (alice, bob, one2oneId) <- createOne2OneConversation OwnDomain
  bobId <- bob %. "qualified_id"

  -- set up an initial "blocked" state
  void $ postConnection bob alice >>= getBody 200
  void $ putConnection alice bob "blocked" >>= getBody 200
  assertConnectionStatus alice bob "blocked"
  getConversation alice one2oneId `bindResponse` \resp ->
    resp.status `shouldMatchInt` 403

  -- If Bob sends a new connection request, Alice ignores it
  void $ postConnection bob alice >>= getBody 200
  assertConnectionStatus alice bob "blocked"

  -- if Alice accepts (or sends a connection request), and Bob still
  -- wants to connect, Alice transitions to "accepted"
  void $ postConnection alice bob >>= getBody 200
  assertConnectionStatus alice bob "accepted"
  getConversation alice one2oneId `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    others <- resp.json %. "members.others" & asList
    qIds <- for others (%. "qualified_id")
    qIds `shouldMatchSet` [bobId]

testSentFromBlocked :: HasCallStack => App ()
testSentFromBlocked = do
  [alice, bob] <- forM [OwnDomain, OtherDomain] $ flip randomUser def
  -- set up an initial "blocked" state
  void $ postConnection bob alice >>= getBody 201
  void $ putConnection alice bob "blocked" >>= getBody 200
  assertConnectionStatus alice bob "blocked"

  -- if Bob rescinds, Alice stays in "blocked"
  void $ putConnection bob alice "cancelled" >>= getBody 200
  assertConnectionStatus alice bob "blocked"

  -- if Alice accepts, and Bob does not want to connect anymore, Alice
  -- transitions to "sent"
  void $ putConnection alice bob "accepted" >>= getBody 200
  assertConnectionStatus alice bob "sent"

testCancel :: HasCallStack => App ()
testCancel = do
  [alice, bob] <- forM [OwnDomain, OtherDomain] $ flip randomUser def

  void $ postConnection alice bob >>= getBody 201
  assertConnectionStatus alice bob "sent"

  void $ putConnection alice bob "cancelled" >>= getBody 200
  assertConnectionStatus alice bob "cancelled"

testConnectionLimits :: HasCallStack => App ()
testConnectionLimits = do
  let connectionLimit = 16

  alice <- randomUser OwnDomain def
  [charlie1, charlie2, charlie3, charlie4] <- replicateM 4 do
    randomUser OtherDomain def
  -- connect to connectionLimit - 1 many users
  (charlie5 : _) <- replicateM (connectionLimit - 1) do
    charlie <- randomUser OtherDomain def
    postConnection alice charlie `bindResponse` \resp ->
      resp.status `shouldMatchInt` 201
    pure charlie

  -- CHARLIE 1

  -- accepting one more connection should be fine
  postConnection charlie1 alice `bindResponse` \resp ->
    resp.status `shouldMatchInt` 201
  putConnection alice charlie1 "accepted" `waitForResponse` \resp ->
    resp.status `shouldMatchInt` 200

  -- resending a connection accept should be idempotent
  putConnection alice charlie1 "accepted" `waitForResponse` \resp ->
    resp.status `shouldMatchInt` 200

  -- CHARLIE 2

  -- an incoming connection beyond the limit should make it
  -- impossible for alice to accept
  postConnection charlie2 alice `bindResponse` \resp ->
    resp.status `shouldMatchInt` 201

  putConnection alice charlie2 "accepted" `waitForResponse` \resp -> do
    resp.json %. "label" `shouldMatch` "connection-limit"
    resp.status `shouldMatchInt` 403

  -- the status should stay pending
  getConnection alice charlie2 `bindResponse` \resp -> do
    resp.json %. "status" `shouldMatch` "pending"
    resp.status `shouldMatchInt` 200

  -- CHARLIE 5

  -- the remote should be able to accept
  putConnection charlie5 alice "accepted" `waitForResponse` \resp ->
    resp.status `shouldMatchInt` 200

  -- the status should change for alice as well
  getConnection alice charlie5 `waitForResponse` \resp -> do
    resp.json %. "status" `shouldMatch` "accepted"
    resp.status `shouldMatchInt` 200

  -- CHARLIE 3

  -- attempting to send a new connection request should also hit the limit
  postConnection alice charlie3 `waitForResponse` \resp -> do
    resp.json %. "label" `shouldMatch` "connection-limit"
    resp.status `shouldMatchInt` 403

  -- CHARLIE 4

  -- blocking should not count towards the connection limit, so after blocking
  -- charlie 1, we should be able establish another connection
  putConnection alice charlie1 "blocked" `bindResponse` \resp ->
    resp.status `shouldMatchInt` 200

  postConnection alice charlie4 `bindResponse` \resp ->
    resp.status `shouldMatchInt` 201

testNonFederatingRemoteTeam :: HasCallStack => App ()
testNonFederatingRemoteTeam =
  withFederatingBackendsAllowDynamic $ \(domainA, domainB, _) -> do
    sequence_
      [ createFedConn domainA (FedConn domainB defSearchPolicy Nothing),
        createFedConn domainB (FedConn domainA defSearchPolicy Nothing)
      ]
    void $ updateFedConn domainA domainB (FedConn domainB defSearchPolicy $ Just [])
    alice <- randomUser domainA def
    bob <- randomUser domainB def
    postConnection alice bob `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 403
      resp.json %. "label" `shouldMatch` "team-not-federating"
  where
    defSearchPolicy = "full_search"

testNonMutualFederationConnectionAttempt :: HasCallStack => App ()
testNonMutualFederationConnectionAttempt =
  withFederatingBackendsAllowDynamic $ \(domainA, domainB, _) -> do
    sequence_
      [ createFedConn domainA (FedConn domainB defSearchPolicy Nothing),
        createFedConn domainB (FedConn domainA defSearchPolicy Nothing)
      ]
    alice <- randomUser domainA def
    bob <- randomUser domainB def {API.BrigInternal.team = True}

    -- Alice's backend federates with Bob's team
    void $ updateFedConn domainA domainB (FedConn domainB defSearchPolicy $ Just [])
    bobTeamId <- bob %. "team"
    addFederationRemoteTeam domainA domainB bobTeamId

    -- Bob's backend federates with no team on Alice's backend
    void $ updateFedConn domainB domainA (FedConn domainA defSearchPolicy $ Just [])

    postConnection alice bob `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 403
      resp.json %. "label" `shouldMatch` "team-not-federating"
  where
    defSearchPolicy = "full_search"

testFederationAllowAllConnectWithRemote :: HasCallStack => App ()
testFederationAllowAllConnectWithRemote =
  withFederatingBackendsAllowDynamic $ \(domainA, domainB, _) -> do
    sequence_
      [ createFedConn domainA (FedConn domainB defSearchPolicy Nothing),
        createFedConn domainB (FedConn domainA defSearchPolicy Nothing)
      ]
    void $ createAndConnectUsers [domainA, domainB]
  where
    defSearchPolicy = "full_search"

testFederationAllowDynamicConnectWithRemote :: HasCallStack => App ()
testFederationAllowDynamicConnectWithRemote =
  withFederatingBackendsAllowDynamic $ \(domainA, domainB, _) -> do
    sequence_
      [ createFedConn domainA (FedConn domainB defSearchPolicy Nothing),
        createFedConn domainB (FedConn domainA defSearchPolicy Nothing)
      ]
    alice <- randomUser domainA def {API.BrigInternal.team = True}
    bob <- randomUser domainB def {API.BrigInternal.team = True}

    -- Alice's backend federates with Bob's team
    void $ updateFedConn domainA domainB (FedConn domainB defSearchPolicy $ Just [])
    bobTeamId <- bob %. "team"
    addFederationRemoteTeam domainA domainB bobTeamId

    -- Bob's backend federates with Alice's team
    void $ updateFedConn domainB domainA (FedConn domainA defSearchPolicy $ Just [])
    aliceTeamId <- alice %. "team"
    addFederationRemoteTeam domainB domainA aliceTeamId

    connectTwoUsers alice bob
  where
    defSearchPolicy = "full_search"

testFederationAllowMixedConnectWithRemote :: HasCallStack => App ()
testFederationAllowMixedConnectWithRemote =
  withFederatingBackendsAllowDynamic $ \(domainA, domainB, _) -> do
    sequence_
      [ createFedConn domainA (FedConn domainB defSearchPolicy Nothing),
        createFedConn domainB (FedConn domainA defSearchPolicy Nothing)
      ]
    alice <- randomUser domainA def {API.BrigInternal.team = True}
    bob <- randomUser domainB def {API.BrigInternal.team = True}

    -- Alice's backend federates with Bob's backend. Bob's backend federates
    -- with Alice's team.
    void $ updateFedConn domainB domainA (FedConn domainA defSearchPolicy $ Just [])
    aliceTeamId <- alice %. "team"
    addFederationRemoteTeam domainB domainA aliceTeamId

    connectTwoUsers alice bob
  where
    defSearchPolicy = "full_search"

testPendingConnectionUserDeleted :: HasCallStack => Domain -> App ()
testPendingConnectionUserDeleted bobsDomain = do
  alice <- randomUser OwnDomain def
  bob <- randomUser bobsDomain def

  withWebSockets [bob] $ \[bobWs] -> do
    void $ postConnection alice bob >>= getBody 201
    void $ awaitMatch (isConnectionNotif "pending") bobWs
    void $ deleteUser alice
    void $ awaitMatch (isConnectionNotif "cancelled") bobWs
