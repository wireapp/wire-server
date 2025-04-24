{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

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

module Test.MLS.Message where

import API.Galley
import API.Gundeck
import MLS.Util
import Notifications
import SetupHelpers
import Testlib.Prelude

-- @SF.Separation @TSFI.RESTfulAPI @S2
-- This test verifies whether a message actually gets sent all the way to
-- cannon.

testApplicationMessage :: (HasCallStack) => App ()
testApplicationMessage = do
  -- Test happy case of federated MLS message sending in both directions.

  -- local alice and alex, remote bob
  [alice, alex, bob, betty] <-
    createUsers
      [OwnDomain, OwnDomain, OtherDomain, OtherDomain]
  for_ [alex, bob, betty] $ \user -> connectTwoUsers alice user

  clients@[alice1, _alice2, alex1, _alex2, bob1, _bob2, _, _] <-
    traverse
      (createMLSClient def)
      [alice, alice, alex, alex, bob, bob, betty, betty]
  traverse_ (uploadNewKeyPackage def) clients
  convId <- createNewGroup def alice1

  withWebSockets [alice, alex, bob, betty] $ \wss -> do
    -- alice adds all other users (including her own client)
    void $ createAddCommit alice1 convId [alice, alex, bob, betty] >>= sendAndConsumeCommitBundle
    traverse_ (awaitMatch isMemberJoinNotif) wss

    -- alex sends a message
    void $ createApplicationMessage convId alex1 "hello" >>= sendAndConsumeMessage
    traverse_ (awaitMatch isNewMLSMessageNotif) wss

    -- bob sends a message
    void $ createApplicationMessage convId bob1 "hey" >>= sendAndConsumeMessage
    traverse_ (awaitMatch isNewMLSMessageNotif) wss

-- @END

testAppMessageSomeReachable :: (HasCallStack) => App ()
testAppMessageSomeReachable = do
  (alice1, convId) <- startDynamicBackends [mempty] $ \[thirdDomain] -> do
    ownDomain <- make OwnDomain & asString
    otherDomain <- make OtherDomain & asString
    [alice, bob, charlie] <- createAndConnectUsers [ownDomain, otherDomain, thirdDomain]

    [alice1, bob1, charlie1] <- traverse (createMLSClient def) [alice, bob, charlie]
    traverse_ (uploadNewKeyPackage def) [bob1, charlie1]
    convId <- createNewGroup def alice1
    void $ withWebSocket charlie $ \ws -> do
      void $ createAddCommit alice1 convId [bob, charlie] >>= sendAndConsumeCommitBundle
      awaitMatch isMemberJoinNotif ws
    pure (alice1, convId)

  -- charlie isn't able to receive this message, so we make sure we can post it
  -- successfully, but not attempt to consume it
  mp <- createApplicationMessage convId alice1 "hi, bob!"
  void $ postMLSMessage mp.sender mp.message >>= getJSON 201

testMessageNotifications :: (HasCallStack) => Domain -> App ()
testMessageNotifications bobDomain = do
  [alice, bob] <- createAndConnectUsers [OwnDomain, bobDomain]

  [alice1, alice2, bob1, bob2] <- traverse (createMLSClient def) [alice, alice, bob, bob]
  bobClient <- bob1 %. "client_id" & asString

  traverse_ (uploadNewKeyPackage def) [alice1, alice2, bob1, bob2]

  convId <- createNewGroup def alice1

  void $ withWebSocket bob $ \ws -> do
    void $ createAddCommit alice1 convId [alice, bob] >>= sendAndConsumeCommitBundle
    awaitMatch isMemberJoinNotif ws

  let get (opts :: GetNotifications) = do
        notifs <- getNotifications bob opts {size = Just 10000} >>= getJSON 200
        notifs %. "has_more" `shouldMatch` False
        length <$> (notifs %. "notifications" & asList)

  numNotifs <- get def
  numNotifsClient <- get def {client = Just bobClient}

  void $ withWebSocket bob $ \ws -> do
    void $ createApplicationMessage convId alice1 "hi bob" >>= sendAndConsumeMessage
    awaitMatch isNewMLSMessageNotif ws

  get def `shouldMatchInt` (numNotifs + 1)
  get def {client = Just bobClient} `shouldMatchInt` (numNotifsClient + 1)

testMultipleMessages :: (HasCallStack) => App ()
testMultipleMessages = do
  [alice, bob] <- createAndConnectUsers [OwnDomain, OtherDomain]
  [alice1, bob1] <- traverse (createMLSClient def) [alice, bob]
  traverse_ (uploadNewKeyPackage def) [alice1, bob1]
  convId <- createNewGroup def alice1

  withWebSockets [bob] $ \wss -> do
    void $ createAddCommit alice1 convId [bob] >>= sendAndConsumeCommitBundle
    traverse_ (awaitMatch isMemberJoinNotif) wss

    void $ createApplicationMessage convId alice1 "hello" >>= sendAndConsumeMessage
    traverse_ (awaitMatch isNewMLSMessageNotif) wss

    void $ createApplicationMessage convId alice1 "world" >>= sendAndConsumeMessage
    traverse_ (awaitMatch isNewMLSMessageNotif) wss
