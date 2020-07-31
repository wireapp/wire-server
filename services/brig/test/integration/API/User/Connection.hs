{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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
import qualified Brig.Options as Opt
import Brig.Types
import Brig.Types.Intra
import Control.Arrow ((&&&))
import Data.Aeson
import Data.ByteString.Conversion
import Data.Id hiding (client)
import qualified Data.UUID.V4 as UUID
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Galley.Types
import Imports
import qualified Network.Wai.Utilities.Error as Error
import Safe hiding (at)
import Test.Tasty hiding (Timeout)
import Test.Tasty.HUnit
import Util

tests :: ConnectionLimit -> Opt.Timeout -> Opt.Opts -> Manager -> Brig -> Cannon -> Galley -> TestTree
tests cl _at _conf p b _c g =
  testGroup
    "connection"
    [ test p "post /connections" $ testCreateManualConnections b,
      test p "post /connections mutual" $ testCreateMutualConnections b g,
      test p "post /connections (bad user)" $ testCreateConnectionInvalidUser b,
      test p "put /connections/:id accept" $ testAcceptConnection b,
      test p "put /connections/:id ignore" $ testIgnoreConnection b,
      test p "put /connections/:id cancel" $ testCancelConnection b,
      test p "put /connections/:id cancel" $ testCancelConnection2 b g,
      test p "put /connections/:id block" $ testBlockConnection b,
      test p "put /connections/:id block-resend" $ testBlockAndResendConnection b g,
      test p "put /connections/:id unblock pending" $ testUnblockPendingConnection b,
      test p "put /connections/:id accept blocked" $ testAcceptWhileBlocked b,
      test p "put /connections/:id bad update" $ testBadUpdateConnection b,
      test p "put /connections/:id noop" $ testUpdateConnectionNoop b,
      test p "get /connections - 200 (paging)" $ testConnectionPaging b,
      test p "post /connections - 400 (max conns)" $ testConnectionLimit b cl,
      test p "post /i/users/auto-connect" $ testAutoConnectionOK b g,
      test p "post /i/users/auto-connect - existing conn" $ testAutoConnectionNoChanges b,
      test p "post /i/users/auto-connect - 400 (bad range)" $ testAutoConnectionBadRequest b
    ]

testCreateConnectionInvalidUser :: Brig -> Http ()
testCreateConnectionInvalidUser brig = do
  uid1 <- userId <$> randomUser brig
  -- user does not exist
  uid2 <- Id <$> liftIO UUID.nextRandom
  postConnection brig uid1 uid2 !!! do
    const 400 === statusCode
    const (Just "invalid-user") === fmap Error.label . responseJsonMaybe
  -- cannot create a connection with yourself
  postConnection brig uid1 uid1 !!! do
    const 400 === statusCode
    const (Just "invalid-user") === fmap Error.label . responseJsonMaybe

testCreateManualConnections :: Brig -> Http ()
testCreateManualConnections brig = do
  uid1 <- userId <$> randomUser brig
  uid2 <- userId <$> randomUser brig
  postConnection brig uid1 uid2 !!! const 201 === statusCode
  assertConnections brig uid1 [ConnectionStatus uid1 uid2 Sent]
  assertConnections brig uid2 [ConnectionStatus uid2 uid1 Pending]
  -- Test that no connections to anonymous users can be created,
  -- as well as that anonymous users cannot create connections.
  uid3 <- userId <$> createAnonUser "foo3" brig
  postConnection brig uid1 uid3 !!! const 400 === statusCode
  postConnection brig uid3 uid1 !!! const 403 === statusCode

testCreateMutualConnections :: Brig -> Galley -> Http ()
testCreateMutualConnections brig galley = do
  uid1 <- userId <$> randomUser brig
  uid2 <- userId <$> randomUser brig
  postConnection brig uid1 uid2 !!! const 201 === statusCode
  assertConnections brig uid1 [ConnectionStatus uid1 uid2 Sent]
  assertConnections brig uid2 [ConnectionStatus uid2 uid1 Pending]
  rsp <- postConnection brig uid2 uid1 <!! const 200 === statusCode
  assertConnections brig uid1 [ConnectionStatus uid1 uid2 Accepted]
  assertConnections brig uid2 [ConnectionStatus uid2 uid1 Accepted]
  case responseJsonMaybe rsp >>= ucConvId of
    Nothing -> liftIO $ assertFailure "incomplete connection"
    Just cnv -> do
      getConversation galley uid1 cnv !!! do
        const 200 === statusCode
        const (Just One2OneConv) === fmap cnvType . responseJsonMaybe
      getConversation galley uid2 cnv !!! do
        const 200 === statusCode
        const (Just One2OneConv) === fmap cnvType . responseJsonMaybe

testAcceptConnection :: Brig -> Http ()
testAcceptConnection brig = do
  uid1 <- userId <$> randomUser brig
  uid2 <- userId <$> randomUser brig
  -- Initiate a new connection (A -> B)
  postConnection brig uid1 uid2 !!! const 201 === statusCode
  -- B accepts
  putConnection brig uid2 uid1 Accepted !!! const 200 === statusCode
  assertConnections brig uid1 [ConnectionStatus uid1 uid2 Accepted]
  assertConnections brig uid2 [ConnectionStatus uid2 uid1 Accepted]
  -- Mutual connection request with a user C
  uid3 <- userId <$> randomUser brig
  postConnection brig uid1 uid3 !!! const 201 === statusCode
  postConnection brig uid3 uid1 !!! const 200 === statusCode
  assertConnections brig uid1 [ConnectionStatus uid1 uid3 Accepted]
  assertConnections brig uid3 [ConnectionStatus uid3 uid1 Accepted]

testIgnoreConnection :: Brig -> Http ()
testIgnoreConnection brig = do
  uid1 <- userId <$> randomUser brig
  uid2 <- userId <$> randomUser brig
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

testCancelConnection :: Brig -> Http ()
testCancelConnection brig = do
  uid1 <- userId <$> randomUser brig
  uid2 <- userId <$> randomUser brig
  -- Initiate a new connection (A -> B)
  postConnection brig uid1 uid2 !!! const 201 === statusCode
  -- A cancels the request
  putConnection brig uid1 uid2 Cancelled !!! const 200 === statusCode
  assertConnections brig uid1 [ConnectionStatus uid1 uid2 Cancelled]
  assertConnections brig uid2 [ConnectionStatus uid2 uid1 Cancelled]
  -- A changes his mind again
  postConnection brig uid1 uid2 !!! const 200 === statusCode
  assertConnections brig uid1 [ConnectionStatus uid1 uid2 Sent]
  assertConnections brig uid2 [ConnectionStatus uid2 uid1 Pending]

testCancelConnection2 :: Brig -> Galley -> Http ()
testCancelConnection2 brig galley = do
  uid1 <- userId <$> randomUser brig
  uid2 <- userId <$> randomUser brig
  -- Initiate a new connection (A -> B)
  postConnection brig uid1 uid2 !!! const 201 === statusCode
  -- A cancels the request
  rsp <- putConnection brig uid1 uid2 Cancelled <!! const 200 === statusCode
  assertConnections brig uid1 [ConnectionStatus uid1 uid2 Cancelled]
  assertConnections brig uid2 [ConnectionStatus uid2 uid1 Cancelled]
  let Just cnv = ucConvId =<< responseJsonMaybe rsp
  -- A cannot see the conversation (due to cancelling)
  getConversation galley uid1 cnv !!! do
    const 403 === statusCode
  -- B cannot see the conversation
  getConversation galley uid2 cnv !!! const 403 === statusCode
  -- B initiates a connection request himself
  postConnection brig uid2 uid1 !!! const 200 === statusCode
  assertConnections brig uid2 [ConnectionStatus uid2 uid1 Sent]
  assertConnections brig uid1 [ConnectionStatus uid1 uid2 Pending]
  -- B is now a current member of the connect conversation
  getConversation galley uid2 cnv !!! do
    const 200 === statusCode
    const (Just ConnectConv) === \rs -> do
      conv <- responseJsonMaybe rs
      Just (cnvType conv)
  -- A is a past member, cannot see the conversation
  getConversation galley uid1 cnv !!! do
    const 403 === statusCode
  -- A finally accepts
  putConnection brig uid1 uid2 Accepted !!! const 200 === statusCode
  assertConnections brig uid1 [ConnectionStatus uid1 uid2 Accepted]
  assertConnections brig uid2 [ConnectionStatus uid2 uid1 Accepted]
  getConversation galley uid1 cnv !!! do
    const 200 === statusCode
  getConversation galley uid2 cnv !!! do
    const 200 === statusCode

testBlockConnection :: Brig -> Http ()
testBlockConnection brig = do
  u1 <- randomUser brig
  u2 <- randomUser brig
  let uid1 = userId u1
  let uid2 = userId u2
  -- Initiate a new connection (A -> B)
  postConnection brig uid1 uid2 !!! const 201 === statusCode
  -- Even connected users cannot see each other's email
  -- (or phone number for that matter).
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

testBlockAndResendConnection :: Brig -> Galley -> Http ()
testBlockAndResendConnection brig galley = do
  u1 <- randomUser brig
  u2 <- randomUser brig
  let uid1 = userId u1
  let uid2 = userId u2
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
  let Just cnv = ucConvId =<< responseJsonMaybe rsp
  getConversation galley uid2 cnv !!! const 403 === statusCode
  -- A can see the conversation and is a current member
  getConversation galley uid1 cnv !!! do
    const 200 === statusCode

testUnblockPendingConnection :: Brig -> Http ()
testUnblockPendingConnection brig = do
  u1 <- userId <$> randomUser brig
  u2 <- userId <$> randomUser brig
  postConnection brig u1 u2 !!! const 201 === statusCode
  putConnection brig u1 u2 Blocked !!! const 200 === statusCode
  assertConnections brig u1 [ConnectionStatus u1 u2 Blocked]
  assertConnections brig u2 [ConnectionStatus u2 u1 Pending]
  putConnection brig u1 u2 Accepted !!! const 200 === statusCode
  assertConnections brig u1 [ConnectionStatus u1 u2 Sent]
  assertConnections brig u2 [ConnectionStatus u2 u1 Pending]

testAcceptWhileBlocked :: Brig -> Http ()
testAcceptWhileBlocked brig = do
  u1 <- userId <$> randomUser brig
  u2 <- userId <$> randomUser brig
  postConnection brig u1 u2 !!! const 201 === statusCode
  putConnection brig u1 u2 Blocked !!! const 200 === statusCode
  assertConnections brig u1 [ConnectionStatus u1 u2 Blocked]
  assertConnections brig u2 [ConnectionStatus u2 u1 Pending]
  putConnection brig u2 u1 Accepted !!! const 200 === statusCode
  assertConnections brig u1 [ConnectionStatus u1 u2 Blocked]
  assertConnections brig u2 [ConnectionStatus u2 u1 Accepted]

testUpdateConnectionNoop :: Brig -> Http ()
testUpdateConnectionNoop brig = do
  u1 <- randomUser brig
  u2 <- randomUser brig
  let uid1 = userId u1
  let uid2 = userId u2
  postConnection brig uid1 uid2 !!! const 201 === statusCode
  putConnection brig uid2 uid1 Accepted !!! const 200 === statusCode
  putConnection brig uid2 uid1 Accepted !!! const 204 === statusCode

testBadUpdateConnection :: Brig -> Http ()
testBadUpdateConnection brig = do
  uid1 <- userId <$> randomUser brig
  uid2 <- userId <$> randomUser brig
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

testConnectionPaging :: Brig -> Http ()
testConnectionPaging b = do
  u <- userId <$> randomUser b
  replicateM_ total $ do
    u2 <- userId <$> randomUser b
    postConnection b u u2 !!! const 201 === statusCode
  foldM_ (next u 2) (0, Nothing) [2, 2, 1, 0]
  foldM_ (next u total) (0, Nothing) [total, 0]
  where
    total = 5
    next u step (count, start) n = do
      let count' = count + step
      let range = queryRange (toByteString' <$> start) (Just step)
      r <-
        get (b . path "/connections" . zUser u . range)
          <!! const 200 === statusCode
      let (conns, more) = (fmap clConnections &&& fmap clHasMore) $ responseJsonMaybe r
      liftIO $ assertEqual "page size" (Just n) (length <$> conns)
      liftIO $ assertEqual "has more" (Just (count' < total)) more
      return . (count',) $ (conns >>= fmap ucTo . listToMaybe . reverse)

testConnectionLimit :: Brig -> ConnectionLimit -> Http ()
testConnectionLimit brig (ConnectionLimit l) = do
  uid1 <- userId <$> randomUser brig
  (uid2 : _) <- replicateM (fromIntegral l) (newConn uid1)
  uidX <- userId <$> randomUser brig
  postConnection brig uid1 uidX !!! assertLimited
  -- blocked connections do not count towards the limit
  putConnection brig uid1 uid2 Blocked !!! const 200 === statusCode
  postConnection brig uid1 uidX !!! const 201 === statusCode
  -- the next send/accept hits the limit again
  uidY <- userId <$> randomUser brig
  postConnection brig uid1 uidY !!! assertLimited
  -- (re-)sending an already accepted connection does not affect the limit
  postConnection brig uid1 uidX !!! const 200 === statusCode
  where
    newConn from = do
      to <- userId <$> randomUser brig
      postConnection brig from to !!! const 201 === statusCode
      return to
    assertLimited = do
      const 403 === statusCode
      const (Just "connection-limit") === fmap Error.label . responseJsonMaybe

testAutoConnectionOK :: Brig -> Galley -> Http ()
testAutoConnectionOK brig galley = do
  uid1 <- userId <$> randomUser brig
  uid2 <- userId <$> randomUser brig
  bdy <-
    postAutoConnection brig uid1 [uid2] <!! do
      const 200 === statusCode
      const (Just 2) === \r -> do
        b <- responseBody r
        Vec.length <$> (decode b :: Maybe (Vector UserConnection))
  assertConnections brig uid1 [ConnectionStatus uid1 uid2 Accepted]
  assertConnections brig uid2 [ConnectionStatus uid2 uid1 Accepted]
  case responseJsonMaybe bdy >>= headMay >>= ucConvId of
    Nothing -> liftIO $ assertFailure "incomplete connection"
    Just cnv -> do
      getConversation galley uid1 cnv !!! do
        const 200 === statusCode
        const (Just One2OneConv) === fmap cnvType . responseJsonMaybe
      getConversation galley uid2 cnv !!! do
        const 200 === statusCode
        const (Just One2OneConv) === fmap cnvType . responseJsonMaybe

testAutoConnectionNoChanges :: Brig -> Http ()
testAutoConnectionNoChanges brig = do
  uid1 <- userId <$> randomUser brig
  uid2 <- userId <$> randomUser brig
  postConnection brig uid1 uid2 !!! const 201 === statusCode
  -- This is effectively a no-op
  postAutoConnection brig uid1 [uid2] !!! do
    const 200 === statusCode
    const (Just 0) === \r -> do
      b <- responseBody r
      Vec.length <$> (decode b :: Maybe (Vector UserConnection))

testAutoConnectionBadRequest :: Brig -> Http ()
testAutoConnectionBadRequest brig = do
  uid1 <- userId <$> randomUser brig
  -- no users
  postAutoConnection brig uid1 [] !!! const 400 === statusCode
  -- too many users
  uids <- replicateM 26 (liftIO $ Id <$> UUID.nextRandom)
  postAutoConnection brig uid1 uids !!! const 400 === statusCode
  -- unactivated / unverified self user
  uid2 <- userId <$> createAnonUser "foo2" brig
  postAutoConnection brig uid2 (take 1 uids) !!! do
    const 403 === statusCode
    const (Just "no-identity") === fmap Error.label . responseJsonMaybe
  -- unactivated / unverified target users simply get filtered out
  postAutoConnection brig uid1 [uid2] !!! do
    const 200 === statusCode
    const (Just 0) === \r -> do
      b <- responseBody r
      Vec.length <$> (decode b :: Maybe (Vector UserConnection))
