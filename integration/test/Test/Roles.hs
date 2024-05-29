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

module Test.Roles where

import API.Galley
import GHC.Stack
import Notifications
import SetupHelpers
import Testlib.Prelude

testRoleUpdateWithRemotesOk :: (HasCallStack) => App ()
testRoleUpdateWithRemotesOk = do
  [bob, charlie, alice] <- createUsers [OwnDomain, OwnDomain, OtherDomain]
  connectTwoUsers bob charlie
  connectTwoUsers bob alice
  conv <-
    postConversation bob (defProteus {qualifiedUsers = [charlie, alice]})
      >>= getJSON 201
  adminRole <- make "wire_admin"

  withWebSockets [bob, charlie, alice] $ \wss -> do
    void $ updateRole bob charlie adminRole conv >>= getBody 200
    bindResponse (getConversation bob conv) $ \resp -> do
      resp.status `shouldMatchInt` 200
      resp.json %. "members.others.0.qualified_id" `shouldMatch` objQidObject charlie
      resp.json %. "members.others.0.conversation_role" `shouldMatch` "wire_admin"
    for_ wss $ \ws -> do
      notif <- awaitMatch isMemberUpdateNotif ws
      notif %. "payload.0.qualified_conversation" `shouldMatch` objQidObject conv
      notif %. "payload.0.qualified_from" `shouldMatch` objQidObject bob

testRoleUpdateWithRemotesUnreachable :: (HasCallStack) => App ()
testRoleUpdateWithRemotesUnreachable = do
  [bob, charlie] <- createUsers [OwnDomain, OwnDomain]
  startDynamicBackends [mempty] $ \[dynBackend] -> do
    alice <- randomUser dynBackend def
    connectTwoUsers bob alice
    connectTwoUsers bob charlie
    conv <-
      postConversation bob (defProteus {qualifiedUsers = [charlie, alice]})
        >>= getJSON 201
    adminRole <- make "wire_admin"

    withWebSockets [bob, charlie] $ \wss -> do
      void $ updateRole bob charlie adminRole conv >>= getBody 200

      for_ wss $ \ws -> do
        notif <- awaitMatch isMemberUpdateNotif ws
        notif %. "payload.0.qualified_conversation" `shouldMatch` objQidObject conv
        notif %. "payload.0.qualified_from" `shouldMatch` objQidObject bob
