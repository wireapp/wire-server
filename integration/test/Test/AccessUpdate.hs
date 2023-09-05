{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

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
import Notifications
import SetupHelpers
import Testlib.Prelude
import Testlib.ResourcePool

-- @SF.Federation @SF.Separation @TSFI.RESTfulAPI @S2
--
-- The test asserts that, among others, remote users are removed from a
-- conversation when an access update occurs that disallows guests from
-- accessing.
testAccessUpdateGuestRemoved :: HasCallStack => App ()
testAccessUpdateGuestRemoved = do
  (alice, tid, [bob]) <- createTeam OwnDomain 2
  charlie <- randomUser OwnDomain def
  dee <- randomUser OtherDomain def
  mapM_ (connectUsers alice) [charlie, dee]
  [aliceClient, bobClient, charlieClient, deeClient] <-
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

  let update = ["access" .= ([] :: [String]), "access_role" .= ["team_member"]]
  void $ updateAccess alice conv update >>= getJSON 200

  mapM_ (assertKickNotification alice conv alice aliceClient) [charlie, dee]
  mapM_ (assertKickNotification alice conv bob bobClient) [charlie, dee]
  mapM_ (assertKickNotification alice conv charlie charlieClient) [charlie, dee]
  mapM_ (assertKickNotification alice conv dee deeClient) [charlie, dee]

  bindResponse (getConversation alice conv) $ \res -> do
    res.status `shouldMatchInt` 200
    res.json %. "members.others.0.qualified_id" `shouldMatch` objQidObject bob

-- @END

testAccessUpdateGuestRemovedUnreachableRemotes :: HasCallStack => App ()
testAccessUpdateGuestRemovedUnreachableRemotes = do
  resourcePool <- asks resourcePool
  (alice, tid, [bob]) <- createTeam OwnDomain 2
  charlie <- randomUser OwnDomain def
  connectUsers alice charlie
  [aliceClient, bobClient, charlieClient] <-
    mapM
      (\user -> objId $ bindResponse (addClient user def) $ getJSON 201)
      [alice, bob, charlie]
  (conv, dee) <- runCodensity (acquireResources 1 resourcePool) $ \[dynBackend] ->
    runCodensity (startDynamicBackend dynBackend mempty) $ \_ -> do
      dee <- randomUser dynBackend.berDomain def
      connectUsers alice dee
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

  mapM_ (assertKickNotification alice conv alice aliceClient) [charlie, dee]
  mapM_ (assertKickNotification alice conv bob bobClient) [charlie, dee]
  mapM_ (assertKickNotification alice conv charlie charlieClient) [charlie, dee]

  bindResponse (getConversation alice conv) $ \res -> do
    res.status `shouldMatchInt` 200
    res.json %. "members.others.0.qualified_id" `shouldMatch` objQidObject bob

--------------------------------------------------------------------------------
-- Utilities

assertKickNotification ::
  ( HasCallStack,
    MakesValue fromUser,
    MakesValue conv,
    MakesValue user,
    MakesValue kickedUser
  ) =>
  fromUser ->
  conv ->
  user ->
  String ->
  kickedUser ->
  App ()
assertKickNotification fromUser conv user client kickedUser =
  void $
    awaitNotification
      user
      client
      noValue
      2
      ( allPreds
          [ isConvLeaveNotif,
            isNotifConv conv,
            isNotifForUser kickedUser,
            isNotifFromUser fromUser
          ]
      )
