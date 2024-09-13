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

module Test.AccessUpdate where

import API.Brig
import API.Galley
import Control.Monad.Codensity
import Control.Monad.Reader
import GHC.Stack
import MLS.Util
import Notifications
import SetupHelpers
import Testlib.Prelude
import Testlib.ResourcePool

-- These two commented out tests exist to test the Setup.hs code.
-- Both of these tests should not appear in the output.

-- testBar :: HasCallStack => App ()
-- testBar = pure ()

{-
testBaz :: HasCallStack => App ()
testBaz = pure ()
-}

data ConversationProtocol
  = ConversationProtocolProteus
  | ConversationProtocolMLS

instance TestCases ConversationProtocol where
  mkTestCases =
    pure
      [ MkTestCase "[proto=proteus]" ConversationProtocolProteus,
        MkTestCase "[proto=mls]" ConversationProtocolMLS
      ]

-- | @SF.Federation @SF.Separation @TSFI.RESTfulAPI @S2
--
-- The test asserts that, among others, remote users are removed from a
-- conversation when an access update occurs that disallows guests from
-- accessing.
testAccessUpdateGuestRemoved :: (HasCallStack) => ConversationProtocol -> App ()
testAccessUpdateGuestRemoved proto = do
  (alice, tid, [bob]) <- createTeam OwnDomain 2
  charlie <- randomUser OwnDomain def
  dee <- randomUser OtherDomain def
  mapM_ (connectTwoUsers alice) [charlie, dee]

  (conv, [aliceClient, bobClient, charlieClient, deeClient]) <- case proto of
    ConversationProtocolProteus -> do
      clients <-
        mapM
          (\user -> objId $ bindResponse (addClient user def) $ getJSON 201)
          [alice, bob, charlie, dee]
      conv <-
        postConversation
          alice
          defProteus
            { qualifiedUsers = [bob, charlie, dee],
              team = Just tid
            }
          >>= getJSON 201
      pure (conv, clients)
    ConversationProtocolMLS -> do
      alice1 <- createMLSClient def alice
      clients <- traverse (createMLSClient def) [bob, charlie, dee]
      traverse_ uploadNewKeyPackage clients

      conv <- postConversation alice1 defMLS {team = Just tid} >>= getJSON 201
      createGroup alice1 conv

      void $ createAddCommit alice1 [bob, charlie, dee] >>= sendAndConsumeCommitBundle
      convId <- conv %. "qualified_id"
      pure (convId, map (.client) (alice1 : clients))

  let update = ["access" .= ([] :: [String]), "access_role" .= ["team_member"]]
  void $ updateAccess alice conv update >>= getJSON 200

  mapM_ (assertLeaveNotification alice conv alice aliceClient) [charlie, dee]
  mapM_ (assertLeaveNotification alice conv bob bobClient) [charlie, dee]
  mapM_ (assertLeaveNotification alice conv charlie charlieClient) [charlie, dee]
  mapM_ (assertLeaveNotification alice conv dee deeClient) [charlie, dee]

  bindResponse (getConversation alice conv) $ \res -> do
    res.status `shouldMatchInt` 200
    res.json %. "members.others.0.qualified_id" `shouldMatch` objQidObject bob

-- @END

testAccessUpdateGuestRemovedUnreachableRemotes :: (HasCallStack) => App ()
testAccessUpdateGuestRemovedUnreachableRemotes = do
  resourcePool <- asks resourcePool
  (alice, tid, [bob]) <- createTeam OwnDomain 2
  charlie <- randomUser OwnDomain def
  connectTwoUsers alice charlie
  [aliceClient, bobClient, charlieClient] <-
    mapM
      (\user -> objId $ bindResponse (addClient user def) $ getJSON 201)
      [alice, bob, charlie]
  (conv, dee) <- runCodensity (acquireResources 1 resourcePool) $ \[dynBackend] ->
    runCodensity (startDynamicBackend dynBackend mempty) $ \_ -> do
      dee <- randomUser dynBackend.berDomain def
      connectTwoUsers alice dee
      conv <-
        postConversation
          alice
          ( defProteus
              { qualifiedUsers = [bob, charlie, dee],
                team = Just tid
              }
          )
          >>= getJSON 201
      pure (conv, dee)

  let update = ["access" .= ([] :: [String]), "access_role" .= ["team_member"]]
  void $ updateAccess alice conv update >>= getJSON 200

  mapM_ (assertLeaveNotification alice conv alice aliceClient) [charlie, dee]
  mapM_ (assertLeaveNotification alice conv bob bobClient) [charlie, dee]
  mapM_ (assertLeaveNotification alice conv charlie charlieClient) [charlie, dee]

  bindResponse (getConversation alice conv) $ \res -> do
    res.status `shouldMatchInt` 200
    res.json %. "members.others.0.qualified_id" `shouldMatch` objQidObject bob

testAccessUpdateWithRemotes :: (HasCallStack) => App ()
testAccessUpdateWithRemotes = do
  [alice, bob, charlie] <- createUsers [OwnDomain, OtherDomain, OwnDomain]
  connectTwoUsers alice bob
  connectTwoUsers alice charlie
  conv <-
    postConversation alice (defProteus {qualifiedUsers = [bob, charlie]})
      >>= getJSON 201
  let update_access_value = ["code"]
      update_access_role_value = ["team_member", "non_team_member", "guest", "service"]
      update = ["access" .= update_access_value, "access_role" .= update_access_role_value]
  withWebSockets [alice, bob, charlie] $ \wss -> do
    void $ updateAccess alice conv update >>= getJSON 200
    for_ wss $ \ws -> do
      notif <- awaitMatch isConvAccessUpdateNotif ws
      notif %. "payload.0.qualified_conversation" `shouldMatch` objQidObject conv
      notif %. "payload.0.qualified_from" `shouldMatch` objQidObject alice
      notif %. "payload.0.data.access" `shouldMatch` update_access_value
      notif %. "payload.0.data.access_role_v2" `shouldMatch` update_access_role_value
