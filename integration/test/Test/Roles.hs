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

module Test.Roles where

import API.Brig
import API.Galley
import Control.Monad.Codensity
import Control.Monad.Reader
import GHC.Stack
import Notifications
import SetupHelpers
import Testlib.Prelude
import Testlib.ResourcePool

testRoleUpdateWithRemotesUnreachable :: HasCallStack => App ()
testRoleUpdateWithRemotesUnreachable = do
  resourcePool <- asks resourcePool
  [bob, charlie] <- createAndConnectUsers [OwnDomain, OwnDomain]
  [bobClient, charlieClient] <-
    forM [bob, charlie] $ \usr ->
      objId $ bindResponse (addClient usr def) $ getJSON 201
  runCodensity (acquireResources 1 resourcePool) $ \[dynBackend] -> do
    (conv, _alice) <-
      runCodensity (startDynamicBackend dynBackend mempty) $ \_ -> do
        alice <- randomUser dynBackend.berDomain def
        mapM_ (connectUsers alice) [bob, charlie]
        conv <-
          postConversation bob (defProteus {qualifiedUsers = [charlie, alice]})
            >>= getJSON 201
        pure (conv, alice)
    adminRole <- make "wire_admin"
    void $ updateRole bob charlie adminRole conv >>= getBody 200

    forBob <- awaitNotification bob bobClient noValue 5 isMemberUpdateNotif
    forCharlie <- awaitNotification charlie charlieClient noValue 5 isMemberUpdateNotif
    forM_ [forBob, forCharlie] $ \notif -> do
      notif %. "payload.0.qualified_conversation" `shouldMatch` objQidObject conv
      notif %. "payload.0.qualified_from" `shouldMatch` objQidObject bob
