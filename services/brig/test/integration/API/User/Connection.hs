{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module API.User.Connection
  ( tests,
  )
where

import API.User.Util
import Bilge hiding (accept, timeout)
import Bilge.Assert
import Brig.Data.Connection (remoteConnectionInsert)
import Brig.Options qualified as Opt
import Cassandra qualified as DB
import Control.Arrow ((&&&))
import Data.ByteString.Conversion
import Data.Domain
import Data.Id
import Data.Json.Util (UTCTimeMillis, toUTCTimeMillis)
import Data.Qualified
import Data.Time.Clock (getCurrentTime)
import Data.UUID.V4 qualified as UUID
import Imports
import Network.Wai.Utilities.Error qualified as Error
import Test.Tasty hiding (Timeout)
import Test.Tasty.HUnit
import Util
import Wire.API.Connection
import Wire.API.Conversation
import Wire.API.Federation.API.Brig
import Wire.API.Federation.Component
import Wire.API.Routes.Internal.Brig.Connection
import Wire.API.Routes.MultiTablePaging
import Wire.API.User as User

tests ::
  ConnectionLimit ->
  Opt.Timeout ->
  Manager ->
  Brig ->
  Cannon ->
  Galley ->
  FedClient 'Brig ->
  DB.ClientState ->
  TestTree
tests cl _at p b _c g fedBrigClient db =
  testGroup
    "connection"
    [ test p "post /connections" $ testCreateManualConnections b,
      test p "post /connections/:domain/:uid" $ testCreateManualConnectionsQualified b,
      test p "post /connections mutual" $ testCreateMutualConnections b g,
      test p "post /connections/:domain/:uid mutual" $ testCreateMutualConnectionsQualified b g,
      test p "post /connections (bad user)" $ testCreateConnectionInvalidUser b,
      test p "post /connections/:domain/:uid (bad user)" $ testCreateConnectionInvalidUserQualified b,
      test p "put /connections/:id accept" $ testAcceptConnection b,
      test p "put /connections/:domain/:id accept" $ testAcceptConnectionQualified b,
      test p "put /connections/:id ignore" $ testIgnoreConnection b,
      test p "put /connections/:domain/:id ignore" $ testIgnoreConnectionQualified b,
      test p "put /connections/:id cancel" $ testCancelConnection b,
      test p "put /connections/:domain/:id cancel" $ testCancelConnectionQualified b,
      test p "put /connections/:id cancel 2" $ testCancelConnection2 b g,
      test p "put /connections/:domain/:id cancel 2" $ testCancelConnectionQualified2 b g,
      test p "put /connections/:id block" $ testBlockConnection b,
      test p "put /connections/:domain/:id block" $ testBlockConnectionQualified b,
      test p "put /connections/:id block-resend" $ testBlockAndResendConnection b g,
      test p "put /connections/:domain/:id block-resend" $ testBlockAndResendConnectionQualified b g,
      test p "put /connections/:id unblock pending" $ testUnblockPendingConnection b,
      test p "put /connections/:domain/:id unblock pending" $ testUnblockPendingConnectionQualified b,
      test p "put /connections/:id accept blocked" $ testAcceptWhileBlocked b,
      test p "put /connections/:domain/:id accept blocked" $ testAcceptWhileBlockedQualified b,
      test p "put /connections/:id bad update" $ testBadUpdateConnection b,
      test p "put /connections/:domain/:id bad update" $ testBadUpdateConnectionQualified b,
      test p "put /connections/:id noop" $ testUpdateConnectionNoop b,
      test p "put /connections/:domain/:id noop" $ testUpdateConnectionNoopQualified b,
      test p "get /connections - 200 (paging)" $ testLocalConnectionsPaging b,
      test p "post /list-connections - 200 (paging)" $ testAllConnectionsPaging b db,
      test p "post /connections - 400 (max conns)" $ testConnectionLimit b cl,
      test p "post /connections/:domain/:id - 400 (max conns)" $ testConnectionLimitQualified b cl,
      test p "Remote connections: connect with no federation" (testConnectFederationNotAvailable b),
      test p "Remote connections: connect OK" (testConnectOK b g fedBrigClient),
      test p "Remote connections: connect with Anon" (testConnectWithAnon b fedBrigClient),
      test p "Remote connections: connection from Anon" (testConnectFromAnon b),
      test p "Remote connections: connect twice" (testConnectFromPending b fedBrigClient)
    ]

testCreateConnectionInvalidUser :: Brig -> Http ()
testCreateConnectionInvalidUser brig = do
  uid1 <- User.userId <$> randomUser brig
  -- user does not exist
  uid2 <- Id <$> liftIO UUID.nextRandom
  postConnection brig uid1 uid2 !!! do
    const 400 === statusCode
    const (Just "invalid-user") === fmap Error.label . responseJsonMaybe
  -- cannot create a connection with yourself
  postConnection brig uid1 uid1 !!! do
    const 400 === statusCode
    const (Just "invalid-user") === fmap Error.label . responseJsonMaybe

testCreateConnectionInvalidUserQualified :: Brig -> Http ()
testCreateConnectionInvalidUserQualified brig = do
  quid1 <- userQualifiedId <$> randomUser brig
  let uid1 = qUnqualified quid1
      domain = qDomain quid1
  -- user does not exist
  uid2 <- Id <$> liftIO UUID.nextRandom
  let quid2 = Qualified uid2 domain
  postConnectionQualified brig uid1 quid2 !!! do
    const 400 === statusCode
    const (Just "invalid-user") === fmap Error.label . responseJsonMaybe
  -- cannot create a connection with yourself
  postConnectionQualified brig uid1 quid1 !!! do
    const 400 === statusCode
    const (Just "invalid-user") === fmap Error.label . responseJsonMaybe

testCreateManualConnections :: Brig -> Http ()
testCreateManualConnections brig = do
  uid1 <- User.userId <$> randomUser brig
  uid2 <- User.userId <$> randomUser brig
  postConnection brig uid1 uid2 !!! const 201 === statusCode
  assertConnections brig uid1 [ConnectionStatus uid1 uid2 Sent]
  assertConnections brig uid2 [ConnectionStatus uid2 uid1 Pending]
  -- Test that no connections to anonymous users can be created,
  -- as well as that anonymous users cannot create connections.
  uid3 <- User.userId <$> createAnonUser "foo3" brig
  postConnection brig uid1 uid3 !!! const 400 === statusCode
  postConnection brig uid3 uid1 !!! const 403 === statusCode

testCreateManualConnectionsQualified :: Brig -> Http ()
testCreateManualConnectionsQualified brig = do
  (quid1, uid1, quid2, uid2) <- twoRandomUsers brig
  postConnectionQualified brig uid1 quid2 !!! const 201 === statusCode
  assertConnectionQualified brig uid1 quid2 Sent
  assertConnectionQualified brig uid2 quid1 Pending
  -- Test that no connections to anonymous users can be created,
  -- as well as that anonymous users cannot create connections.
  quid3 <- userQualifiedId <$> createAnonUser "foo3" brig
  let uid3 = qUnqualified quid3
  postConnectionQualified brig uid1 quid3 !!! const 400 === statusCode
  postConnectionQualified brig uid3 quid1 !!! const 403 === statusCode

testCreateMutualConnections :: Brig -> Galley -> Http ()
testCreateMutualConnections brig galley = do
  uid1 <- User.userId <$> randomUser brig
  uid2 <- User.userId <$> randomUser brig
  postConnection brig uid1 uid2 !!! const 201 === statusCode
  assertConnections brig uid1 [ConnectionStatus uid1 uid2 Sent]
  assertConnections brig uid2 [ConnectionStatus uid2 uid1 Pending]
  rsp <- postConnection brig uid2 uid1 <!! const 200 === statusCode
  assertConnections brig uid1 [ConnectionStatus uid1 uid2 Accepted]
  assertConnections brig uid2 [ConnectionStatus uid2 uid1 Accepted]
  case responseJsonMaybe rsp >>= ucConvId of
    Nothing -> liftIO $ assertFailure "incomplete connection"
    Just qcnv -> do
      getConversationQualified galley uid1 qcnv !!! do
        const 200 === statusCode
        const (Just One2OneConv) === fmap cnvType . responseJsonMaybe
      getConversationQualified galley uid2 qcnv !!! do
        const 200 === statusCode
        const (Just One2OneConv) === fmap cnvType . responseJsonMaybe

testCreateMutualConnectionsQualified :: Brig -> Galley -> Http ()
testCreateMutualConnectionsQualified brig galley = do
  (quid1, uid1, quid2, uid2) <- twoRandomUsers brig

  postConnectionQualified brig uid1 quid2 !!! const 201 === statusCode
  assertConnectionQualified brig uid1 quid2 Sent
  assertConnectionQualified brig uid2 quid1 Pending

  rsp <- postConnectionQualified brig uid2 quid1 <!! const 200 === statusCode
  assertConnectionQualified brig uid2 quid1 Accepted
  assertConnectionQualified brig uid1 quid2 Accepted

  case responseJsonMaybe rsp >>= ucConvId of
    Nothing -> liftIO $ assertFailure "incomplete connection"
    Just cnv -> do
      getConversationQualified galley uid1 cnv !!! do
        const 200 === statusCode
        const (Just One2OneConv) === fmap cnvType . responseJsonMaybe
      getConversationQualified galley uid2 cnv !!! do
        const 200 === statusCode
        const (Just One2OneConv) === fmap cnvType . responseJsonMaybe

testAcceptConnection :: Brig -> Http ()
testAcceptConnection brig = do
  uid1 <- User.userId <$> randomUser brig
  uid2 <- User.userId <$> randomUser brig
  -- Initiate a new connection (A -> B)
  postConnection brig uid1 uid2 !!! const 201 === statusCode
  -- B accepts
  putConnection brig uid2 uid1 Accepted !!! const 200 === statusCode
  assertConnections brig uid1 [ConnectionStatus uid1 uid2 Accepted]
  assertConnections brig uid2 [ConnectionStatus uid2 uid1 Accepted]
  -- Mutual connection request with a user C
  uid3 <- User.userId <$> randomUser brig
  postConnection brig uid1 uid3 !!! const 201 === statusCode
  postConnection brig uid3 uid1 !!! const 200 === statusCode
  assertConnections brig uid1 [ConnectionStatus uid1 uid3 Accepted]
  assertConnections brig uid3 [ConnectionStatus uid3 uid1 Accepted]

testAcceptConnectionQualified :: Brig -> Http ()
testAcceptConnectionQualified brig = do
  (quid1, uid1, quid2, uid2) <- twoRandomUsers brig
  -- Initiate a new connection (A -> B)
  postConnectionQualified brig uid1 quid2 !!! const 201 === statusCode
  -- B accepts
  putConnectionQualified brig uid2 quid1 Accepted !!! const 200 === statusCode

  assertConnectionQualified brig uid1 quid2 Accepted
  assertConnectionQualified brig uid2 quid1 Accepted

testIgnoreConnection :: Brig -> Http ()
testIgnoreConnection brig = do
  uid1 <- User.userId <$> randomUser brig
  uid2 <- User.userId <$> randomUser brig
  -- Initiate a new connection (A -> B)
  postConnection brig uid1 uid2 !!! const 201 === statusCode
  -- B ignores A
  putConnection brig uid2 uid1 Ignored !!! const 200 === statusCode
  assertConnections brig uid1 [ConnectionStatus uid1 uid2 Sent]
  assertConnections brig uid2 [ConnectionStatus uid2 uid1 Ignored]
  -- B accepts after all
  putConnection brig uid2 uid1 Accepted !!! const 200 === statusCode
  assertConnections brig uid1 [ConnectionStatus uid1 uid2 Accepted]
  assertConnections brig uid2 [ConnectionStatus uid2 uid1 Accepted]

testIgnoreConnectionQualified :: Brig -> Http ()
testIgnoreConnectionQualified brig = do
  (quid1, uid1, quid2, uid2) <- twoRandomUsers brig
  -- Initiate a new connection (A -> B)
  postConnectionQualified brig uid1 quid2 !!! const 201 === statusCode
  -- B ignores A
  putConnectionQualified brig uid2 quid1 Ignored !!! const 200 === statusCode
  assertConnectionQualified brig uid1 quid2 Sent
  assertConnectionQualified brig uid2 quid1 Ignored
  -- B accepts after all
  putConnectionQualified brig uid2 quid1 Accepted !!! const 200 === statusCode
  assertConnectionQualified brig uid1 quid2 Accepted
  assertConnectionQualified brig uid2 quid1 Accepted

testCancelConnection :: Brig -> Http ()
testCancelConnection brig = do
  uid1 <- User.userId <$> randomUser brig
  uid2 <- User.userId <$> randomUser brig
  -- Initiate a new connection (A -> B)
  postConnection brig uid1 uid2 !!! const 201 === statusCode
  -- A cancels the request
  putConnection brig uid1 uid2 Cancelled !!! const 200 === statusCode
  assertConnections brig uid1 [ConnectionStatus uid1 uid2 Cancelled]
  assertConnections brig uid2 [ConnectionStatus uid2 uid1 Cancelled]
  -- A changes their mind again
  postConnection brig uid1 uid2 !!! const 200 === statusCode
  assertConnections brig uid1 [ConnectionStatus uid1 uid2 Sent]
  assertConnections brig uid2 [ConnectionStatus uid2 uid1 Pending]

testCancelConnectionQualified :: Brig -> Http ()
testCancelConnectionQualified brig = do
  (quid1, uid1, quid2, uid2) <- twoRandomUsers brig
  -- Initiate a new connection (A -> B)
  postConnectionQualified brig uid1 quid2 !!! const 201 === statusCode
  -- A cancels the request
  putConnectionQualified brig uid1 quid2 Cancelled !!! const 200 === statusCode
  assertConnectionQualified brig uid1 quid2 Cancelled
  assertConnectionQualified brig uid2 quid1 Cancelled
  -- A changes their mind again
  postConnectionQualified brig uid1 quid2 !!! const 200 === statusCode
  assertConnectionQualified brig uid1 quid2 Sent
  assertConnectionQualified brig uid2 quid1 Pending

testCancelConnection2 :: Brig -> Galley -> Http ()
testCancelConnection2 brig galley = do
  uid1 <- User.userId <$> randomUser brig
  uid2 <- User.userId <$> randomUser brig
  -- Initiate a new connection (A -> B)
  postConnection brig uid1 uid2 !!! const 201 === statusCode
  -- A cancels the request
  rsp <- putConnection brig uid1 uid2 Cancelled <!! const 200 === statusCode
  assertConnections brig uid1 [ConnectionStatus uid1 uid2 Cancelled]
  assertConnections brig uid2 [ConnectionStatus uid2 uid1 Cancelled]
  let Just qcnv = ucConvId =<< responseJsonMaybe rsp
  -- A cannot see the conversation (due to cancelling)
  getConversationQualified galley uid1 qcnv !!! do
    const 403 === statusCode
  -- B cannot see the conversation
  getConversationQualified galley uid2 qcnv !!! const 403 === statusCode
  -- B initiates a connection request himself
  postConnection brig uid2 uid1 !!! const 200 === statusCode
  assertConnections brig uid2 [ConnectionStatus uid2 uid1 Sent]
  assertConnections brig uid1 [ConnectionStatus uid1 uid2 Pending]
  -- B is now a current member of the connect conversation
  getConversationQualified galley uid2 qcnv !!! do
    const 200 === statusCode
    const (Just ConnectConv) === \rs -> do
      conv <- responseJsonMaybe rs
      Just (cnvType conv)
  -- A is a past member, cannot see the conversation
  getConversationQualified galley uid1 qcnv !!! do
    const 403 === statusCode
  -- A finally accepts
  putConnection brig uid1 uid2 Accepted !!! const 200 === statusCode
  assertConnections brig uid1 [ConnectionStatus uid1 uid2 Accepted]
  assertConnections brig uid2 [ConnectionStatus uid2 uid1 Accepted]
  getConversationQualified galley uid1 qcnv !!! do
    const 200 === statusCode
  getConversationQualified galley uid2 qcnv !!! do
    const 200 === statusCode

testCancelConnectionQualified2 :: Brig -> Galley -> Http ()
testCancelConnectionQualified2 brig galley = do
  (quid1, uid1, quid2, uid2) <- twoRandomUsers brig
  -- Initiate a new connection (A -> B)
  postConnectionQualified brig uid1 quid2 !!! const 201 === statusCode
  -- A cancels the request
  rsp <- putConnectionQualified brig uid1 quid2 Cancelled <!! const 200 === statusCode
  assertConnectionQualified brig uid1 quid2 Cancelled
  assertConnectionQualified brig uid2 quid1 Cancelled
  let Just cnv = ucConvId =<< responseJsonMaybe rsp
  -- A cannot see the conversation (due to cancelling)
  getConversationQualified galley uid1 cnv !!! do
    const 403 === statusCode
  -- B cannot see the conversation
  getConversationQualified galley uid2 cnv !!! const 403 === statusCode
  -- B inititates a connection request themselves
  postConnectionQualified brig uid2 quid1 !!! const 200 === statusCode
  assertConnectionQualified brig uid2 quid1 Sent
  assertConnectionQualified brig uid1 quid2 Pending
  -- B is now a current member of the connect conversation
  getConversationQualified galley uid2 cnv !!! do
    const 200 === statusCode
    const (Just ConnectConv) === \rs -> do
      conv <- responseJsonMaybe rs
      Just (cnvType conv)
  -- A is a past member, cannot see the conversation
  getConversationQualified galley uid1 cnv !!! do
    const 403 === statusCode
  -- A finally accepts
  putConnectionQualified brig uid1 quid2 Accepted !!! const 200 === statusCode
  assertConnectionQualified brig uid2 quid1 Accepted
  assertConnectionQualified brig uid1 quid2 Accepted
  getConversationQualified galley uid1 cnv !!! do
    const 200 === statusCode
  getConversationQualified galley uid2 cnv !!! do
    const 200 === statusCode

testBlockConnection :: Brig -> Http ()
testBlockConnection brig = do
  u1 <- randomUser brig
  u2 <- randomUser brig
  let uid1 = User.userId u1
  let uid2 = User.userId u2
  -- Initiate a new connection (A -> B)
  postConnection brig uid1 uid2 !!! const 201 === statusCode
  -- Even connected users cannot see each other's email
  assertEmailVisibility brig u2 u1 False
  assertEmailVisibility brig u1 u2 False
  -- B blocks A
  putConnection brig uid2 uid1 Blocked !!! const 200 === statusCode
  -- A does not notice that he got blocked
  postConnection brig uid1 uid2 !!! do
    const 200 === statusCode
    const (Just Sent) === fmap ucStatus . responseJsonMaybe
  assertConnections brig uid2 [ConnectionStatus uid2 uid1 Blocked]
  -- B accepts after all
  putConnection brig uid2 uid1 Accepted !!! const 200 === statusCode
  assertConnections brig uid1 [ConnectionStatus uid1 uid2 Accepted]
  assertConnections brig uid2 [ConnectionStatus uid2 uid1 Accepted]
  assertEmailVisibility brig u1 u2 False
  -- B blocks A again
  putConnection brig uid2 uid1 Blocked !!! const 200 === statusCode
  assertConnections brig uid2 [ConnectionStatus uid2 uid1 Blocked]
  assertConnections brig uid1 [ConnectionStatus uid1 uid2 Accepted]
  assertEmailVisibility brig u1 u2 False
  -- B accepts again
  putConnection brig uid2 uid1 Accepted !!! const 200 === statusCode
  assertConnections brig uid1 [ConnectionStatus uid1 uid2 Accepted]
  assertConnections brig uid2 [ConnectionStatus uid2 uid1 Accepted]
  assertEmailVisibility brig u1 u2 False
  -- A blocks B
  putConnection brig uid1 uid2 Blocked !!! const 200 === statusCode
  assertConnections brig uid1 [ConnectionStatus uid1 uid2 Blocked]
  assertConnections brig uid2 [ConnectionStatus uid2 uid1 Accepted]
  assertEmailVisibility brig u2 u1 False
  -- A accepts B again
  putConnection brig uid1 uid2 Accepted !!! const 200 === statusCode
  assertConnections brig uid2 [ConnectionStatus uid2 uid1 Accepted]
  assertConnections brig uid1 [ConnectionStatus uid1 uid2 Accepted]
  assertEmailVisibility brig u2 u1 False

testBlockConnectionQualified :: Brig -> Http ()
testBlockConnectionQualified brig = do
  u1 <- randomUser brig
  u2 <- randomUser brig
  let uid1 = User.userId u1
      uid2 = User.userId u2
      quid1 = userQualifiedId u1
      quid2 = userQualifiedId u2
  -- Initiate a new connection (A -> B)
  postConnectionQualified brig uid1 quid2 !!! const 201 === statusCode
  -- Even connected users cannot see each other's email
  assertEmailVisibility brig u2 u1 False
  assertEmailVisibility brig u1 u2 False
  -- B blocks A
  putConnectionQualified brig uid2 quid1 Blocked !!! const 200 === statusCode
  -- A does not notice that he got blocked
  postConnectionQualified brig uid1 quid2 !!! do
    const 200 === statusCode
    const (Just Sent) === fmap ucStatus . responseJsonMaybe
  assertConnectionQualified brig uid2 quid1 Blocked
  -- B accepts after all
  putConnectionQualified brig uid2 quid1 Accepted !!! const 200 === statusCode
  assertConnectionQualified brig uid1 quid2 Accepted
  assertConnectionQualified brig uid2 quid1 Accepted
  assertEmailVisibility brig u1 u2 False
  -- B blocks A again
  putConnectionQualified brig uid2 quid1 Blocked !!! const 200 === statusCode
  assertConnectionQualified brig uid2 quid1 Blocked
  assertConnectionQualified brig uid1 quid2 Accepted
  assertEmailVisibility brig u1 u2 False
  -- B accepts again
  putConnectionQualified brig uid2 quid1 Accepted !!! const 200 === statusCode
  assertConnectionQualified brig uid1 quid2 Accepted
  assertConnectionQualified brig uid2 quid1 Accepted
  assertEmailVisibility brig u1 u2 False
  -- A blocks B
  putConnectionQualified brig uid1 quid2 Blocked !!! const 200 === statusCode
  assertConnectionQualified brig uid1 quid2 Blocked
  assertConnectionQualified brig uid2 quid1 Accepted
  assertEmailVisibility brig u2 u1 False
  -- A accepts B again
  putConnectionQualified brig uid1 quid2 Accepted !!! const 200 === statusCode
  assertConnectionQualified brig uid1 quid2 Accepted
  assertConnectionQualified brig uid2 quid1 Accepted
  assertEmailVisibility brig u2 u1 False

testBlockAndResendConnection :: Brig -> Galley -> Http ()
testBlockAndResendConnection brig galley = do
  u1 <- randomUser brig
  u2 <- randomUser brig
  let uid1 = User.userId u1
  let uid2 = User.userId u2
  -- Initiate a new connection (A -> B)
  postConnection brig uid1 uid2 !!! const 201 === statusCode
  -- B blocks A
  putConnection brig uid2 uid1 Blocked !!! const 200 === statusCode
  -- A blocks B
  putConnection brig uid1 uid2 Blocked !!! const 200 === statusCode
  -- Cannot resend while blocked, need to unblock first
  postConnection brig uid1 uid2 !!! const 403 === statusCode
  -- Unblock
  putConnection brig uid1 uid2 Accepted !!! const 200 === statusCode
  -- Try to resend the connection request
  -- B is not actually notified, since he blocked.
  rsp <- postConnection brig uid1 uid2 <!! const 200 === statusCode
  assertConnections brig uid1 [ConnectionStatus uid1 uid2 Accepted]
  assertConnections brig uid2 [ConnectionStatus uid2 uid1 Blocked]
  -- B never accepted and thus does not see the conversation
  let Just qcnv = ucConvId =<< responseJsonMaybe rsp
  getConversationQualified galley uid2 qcnv !!! const 403 === statusCode
  -- A can see the conversation and is a current member
  getConversationQualified galley uid1 qcnv !!! do
    const 200 === statusCode

testBlockAndResendConnectionQualified :: Brig -> Galley -> Http ()
testBlockAndResendConnectionQualified brig galley = do
  (quid1, uid1, quid2, uid2) <- twoRandomUsers brig
  -- Initiate a new connection (A -> B)
  postConnectionQualified brig uid1 quid2 !!! const 201 === statusCode
  -- B blocks A
  putConnectionQualified brig uid2 quid1 Blocked !!! const 200 === statusCode
  -- A blocks B
  putConnectionQualified brig uid1 quid2 Blocked !!! const 200 === statusCode
  -- Cannot resend while blocked, need to unblock first
  postConnectionQualified brig uid1 quid2 !!! const 403 === statusCode
  -- Unblock
  putConnectionQualified brig uid1 quid2 Accepted !!! const 200 === statusCode
  -- Try to resend the connection request
  -- B is not actually notified, since he blocked.
  rsp <- postConnectionQualified brig uid1 quid2 <!! const 200 === statusCode
  assertConnectionQualified brig uid1 quid2 Accepted
  assertConnectionQualified brig uid2 quid1 Blocked
  -- B never accepted and thus does not see the conversation
  let Just cnv = ucConvId =<< responseJsonMaybe rsp
  getConversationQualified galley uid2 cnv !!! const 403 === statusCode
  -- A can see the conversation and is a current member
  getConversationQualified galley uid1 cnv !!! do
    const 200 === statusCode

testUnblockPendingConnection :: Brig -> Http ()
testUnblockPendingConnection brig = do
  u1 <- User.userId <$> randomUser brig
  u2 <- User.userId <$> randomUser brig
  postConnection brig u1 u2 !!! const 201 === statusCode
  putConnection brig u1 u2 Blocked !!! const 200 === statusCode
  assertConnections brig u1 [ConnectionStatus u1 u2 Blocked]
  assertConnections brig u2 [ConnectionStatus u2 u1 Pending]
  putConnection brig u1 u2 Accepted !!! const 200 === statusCode
  assertConnections brig u1 [ConnectionStatus u1 u2 Sent]
  assertConnections brig u2 [ConnectionStatus u2 u1 Pending]

testUnblockPendingConnectionQualified :: Brig -> Http ()
testUnblockPendingConnectionQualified brig = do
  (quid1, uid1, quid2, uid2) <- twoRandomUsers brig
  postConnectionQualified brig uid1 quid2 !!! const 201 === statusCode
  putConnectionQualified brig uid1 quid2 Blocked !!! const 200 === statusCode
  assertConnectionQualified brig uid1 quid2 Blocked
  assertConnectionQualified brig uid2 quid1 Pending
  putConnectionQualified brig uid1 quid2 Accepted !!! const 200 === statusCode
  assertConnectionQualified brig uid1 quid2 Sent
  assertConnectionQualified brig uid2 quid1 Pending

testAcceptWhileBlocked :: Brig -> Http ()
testAcceptWhileBlocked brig = do
  u1 <- User.userId <$> randomUser brig
  u2 <- User.userId <$> randomUser brig
  postConnection brig u1 u2 !!! const 201 === statusCode
  putConnection brig u1 u2 Blocked !!! const 200 === statusCode
  assertConnections brig u1 [ConnectionStatus u1 u2 Blocked]
  assertConnections brig u2 [ConnectionStatus u2 u1 Pending]
  putConnection brig u2 u1 Accepted !!! const 200 === statusCode
  assertConnections brig u1 [ConnectionStatus u1 u2 Blocked]
  assertConnections brig u2 [ConnectionStatus u2 u1 Accepted]

testAcceptWhileBlockedQualified :: Brig -> Http ()
testAcceptWhileBlockedQualified brig = do
  (quid1, uid1, quid2, uid2) <- twoRandomUsers brig
  postConnectionQualified brig uid1 quid2 !!! const 201 === statusCode
  putConnectionQualified brig uid1 quid2 Blocked !!! const 200 === statusCode
  assertConnectionQualified brig uid1 quid2 Blocked
  assertConnectionQualified brig uid2 quid1 Pending
  putConnectionQualified brig uid2 quid1 Accepted !!! const 200 === statusCode
  assertConnectionQualified brig uid1 quid2 Blocked
  assertConnectionQualified brig uid2 quid1 Accepted

testUpdateConnectionNoop :: Brig -> Http ()
testUpdateConnectionNoop brig = do
  (_, uid1, _, uid2) <- twoRandomUsers brig
  postConnection brig uid1 uid2 !!! const 201 === statusCode
  putConnection brig uid2 uid1 Accepted !!! const 200 === statusCode
  putConnection brig uid2 uid1 Accepted !!! const 204 === statusCode

testUpdateConnectionNoopQualified :: Brig -> Http ()
testUpdateConnectionNoopQualified brig = do
  (quid1, uid1, quid2, uid2) <- twoRandomUsers brig
  postConnectionQualified brig uid1 quid2 !!! const 201 === statusCode
  putConnectionQualified brig uid2 quid1 Accepted !!! const 200 === statusCode
  putConnectionQualified brig uid2 quid1 Accepted !!! const 204 === statusCode

testBadUpdateConnection :: Brig -> Http ()
testBadUpdateConnection brig = do
  uid1 <- User.userId <$> randomUser brig
  uid2 <- User.userId <$> randomUser brig
  postConnection brig uid1 uid2 !!! const 201 === statusCode
  assertBadUpdate uid1 uid2 Pending
  assertBadUpdate uid1 uid2 Ignored
  assertBadUpdate uid1 uid2 Accepted
  assertBadUpdate uid2 uid1 Sent
  where
    assertBadUpdate u1 u2 s =
      putConnection brig u1 u2 s !!! do
        const 403 === statusCode
        const (Just "bad-conn-update") === fmap Error.label . responseJsonMaybe

testBadUpdateConnectionQualified :: Brig -> Http ()
testBadUpdateConnectionQualified brig = do
  (quid1, uid1, quid2, uid2) <- twoRandomUsers brig
  postConnectionQualified brig uid1 quid2 !!! const 201 === statusCode
  assertBadUpdate uid1 quid2 Pending
  assertBadUpdate uid1 quid2 Ignored
  assertBadUpdate uid1 quid2 Accepted
  assertBadUpdate uid2 quid1 Sent
  where
    assertBadUpdate :: UserId -> Qualified UserId -> Relation -> Http ()
    assertBadUpdate uid1 quid2 s =
      putConnectionQualified brig uid1 quid2 s !!! do
        const 403 === statusCode
        const (Just "bad-conn-update") === fmap Error.label . responseJsonMaybe

testLocalConnectionsPaging :: Brig -> Http ()
testLocalConnectionsPaging b = do
  u <- User.userId <$> randomUser b
  replicateM_ total $ do
    u2 <- User.userId <$> randomUser b
    postConnection b u u2 !!! const 201 === statusCode
  foldM_ (next u 2) (0, Nothing) [2, 2, 1, 0]
  foldM_ (next u total) (0, Nothing) [total, 0]
  where
    total = 5
    next :: UserId -> Int -> (Int, Maybe UserId) -> Int -> HttpT IO (Int, Maybe UserId)
    next u step (count, start) n = do
      let count' = count + step
      let range = queryRange (toByteString' <$> start) (Just step)
      r <-
        get (apiVersion "v1" . b . path "/connections" . zUser u . range)
          <!! const 200 === statusCode
      let (conns, more) = (fmap clConnections &&& fmap clHasMore) $ responseJsonMaybe r
      liftIO $ assertEqual "page size" (Just n) (length <$> conns)
      liftIO $ assertEqual "has more" (Just (count' < total)) more
      pure (count', conns >>= fmap (qUnqualified . ucTo) . listToMaybe . reverse)

testAllConnectionsPaging :: Brig -> DB.ClientState -> Http ()
testAllConnectionsPaging b db = do
  quid <- userQualifiedId <$> randomUser b
  let uid = qUnqualified quid
  replicateM_ totalLocal $ do
    qOther <- userQualifiedId <$> randomUser b
    postConnectionQualified b uid qOther !!! const 201 === statusCode

  -- FUTUREWORK: For now, because we do not support creating remote connections
  -- yet (as of Oct 1, 2021), we write some made-up remote connections directly
  -- to the database such that querying works.
  now <- toUTCTimeMillis <$> liftIO getCurrentTime
  replicateM_ totalRemote $ createRemoteConnection uid now

  -- get all connections at once
  resAll :: ConnectionsPage <- responseJsonError =<< listAllConnections b uid Nothing Nothing
  liftIO $ assertEqual "all: size" total (length . mtpResults $ resAll)
  liftIO $ assertEqual "all: has_more" False (mtpHasMore resAll)

  -- paginate by passing the pagingState
  resFirst :: ConnectionsPage <- responseJsonError =<< listAllConnections b uid (Just size) Nothing
  liftIO $ assertEqual "first: size" size (length . mtpResults $ resFirst)
  liftIO $ assertEqual "first: has_more" True (mtpHasMore resFirst)

  resNext :: ConnectionsPage <- responseJsonError =<< listAllConnections b uid Nothing (Just $ mtpPagingState resFirst)
  liftIO $ assertEqual "next: size" (total - size) (length . mtpResults $ resNext)
  liftIO $ assertEqual "next: has_more" False (mtpHasMore resNext)
  where
    size = 2
    totalLocal = 5
    totalRemote = 3
    total = totalLocal + totalRemote
    remoteDomain = Domain "faraway.example.com"
    createRemoteConnection :: UserId -> UTCTimeMillis -> Http ()
    createRemoteConnection self now = do
      qOther <- (`Qualified` remoteDomain) <$> randomId
      qConv <- (`Qualified` remoteDomain) <$> randomId
      liftIO . DB.runClient db $
        DB.retry DB.x5 $
          DB.write remoteConnectionInsert $
            DB.params
              DB.LocalQuorum
              (self, remoteDomain, qUnqualified qOther, SentWithHistory, now, qDomain qConv, qUnqualified qConv)

testConnectionLimit :: Brig -> ConnectionLimit -> Http ()
testConnectionLimit brig (ConnectionLimit l) = do
  uid1 <- User.userId <$> randomUser brig
  (uid2 : _) <- replicateM (fromIntegral l) (newConn uid1)
  uidX <- User.userId <$> randomUser brig
  postConnection brig uid1 uidX !!! assertLimited
  -- blocked connections do not count towards the limit
  putConnection brig uid1 uid2 Blocked !!! const 200 === statusCode
  postConnection brig uid1 uidX !!! const 201 === statusCode
  -- the next send/accept hits the limit again
  uidY <- User.userId <$> randomUser brig
  postConnection brig uid1 uidY !!! assertLimited
  -- (re-)sending an already accepted connection does not affect the limit
  postConnection brig uid1 uidX !!! const 200 === statusCode
  where
    newConn from = do
      to <- User.userId <$> randomUser brig
      postConnection brig from to !!! const 201 === statusCode
      pure to
    assertLimited = do
      const 403 === statusCode
      const (Just "connection-limit") === fmap Error.label . responseJsonMaybe

testConnectionLimitQualified :: Brig -> ConnectionLimit -> Http ()
testConnectionLimitQualified brig (ConnectionLimit l) = do
  quid1 <- userQualifiedId <$> randomUser brig
  let uid1 = qUnqualified quid1
  (quid2 : _) <- replicateM (fromIntegral l) (newConn uid1)
  quidX <- userQualifiedId <$> randomUser brig
  postConnectionQualified brig uid1 quidX !!! assertLimited
  -- blocked connections do not count towards the limit
  putConnectionQualified brig uid1 quid2 Blocked !!! const 200 === statusCode
  postConnectionQualified brig uid1 quidX !!! const 201 === statusCode
  -- the next send/accept hits the limit again
  quidY <- userQualifiedId <$> randomUser brig
  postConnectionQualified brig uid1 quidY !!! assertLimited
  -- (re-)sending an already accepted connection does not affect the limit
  postConnectionQualified brig uid1 quidX !!! const 200 === statusCode
  where
    newConn :: UserId -> Http (Qualified UserId)
    newConn from = do
      to <- userQualifiedId <$> randomUser brig
      postConnectionQualified brig from to !!! const 201 === statusCode
      pure to
    assertLimited = do
      const 403 === statusCode
      const (Just "connection-limit") === fmap Error.label . responseJsonMaybe

testConnectFederationNotAvailable :: Brig -> Http ()
testConnectFederationNotAvailable brig = do
  (uid1, quid2) <- localAndRemoteUser brig
  postConnectionQualified brig uid1 quid2
    !!! const 422 === statusCode

testConnectOK :: Brig -> Galley -> FedClient 'Brig -> Http ()
testConnectOK brig galley fedBrigClient = do
  let convIsLocal = True
  (uid1, quid2, convId) <- localAndRemoteUserWithConvId brig convIsLocal
  receiveConnectionAction brig fedBrigClient uid1 quid2 RemoteConnect Nothing Pending

  -- The conversation exists uid1 is not a participant however
  getConversationQualified galley uid1 convId
    !!! statusCode === const 403

testConnectWithAnon :: Brig -> FedClient 'Brig -> Http ()
testConnectWithAnon brig fedBrigClient = do
  fromUser <- randomId
  toUser <- User.userId <$> createAnonUser "anon1234" brig
  res <-
    runFedClient @"send-connection-action" fedBrigClient (Domain "far-away.example.com") $
      NewConnectionRequest fromUser Nothing toUser RemoteConnect
  liftIO $
    assertEqual "The response should specify that the user is not activated" NewConnectionResponseUserNotActivated res

testConnectFromAnon :: Brig -> Http ()
testConnectFromAnon brig = do
  anonUser <- User.userId <$> createAnonUser "anon1234" brig
  remoteUser <- fakeRemoteUser
  postConnectionQualified brig anonUser remoteUser !!! const 403 === statusCode

testConnectFromPending :: Brig -> FedClient 'Brig -> Http ()
testConnectFromPending brig fedBrigClient = do
  (uid1, quid2) <- localAndRemoteUser brig
  receiveConnectionAction brig fedBrigClient uid1 quid2 RemoteConnect Nothing Pending
  receiveConnectionAction brig fedBrigClient uid1 quid2 RemoteConnect Nothing Pending
  receiveConnectionAction brig fedBrigClient uid1 quid2 RemoteRescind Nothing Cancelled
