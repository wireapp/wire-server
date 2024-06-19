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

module Test.MessageTimer where

import API.Galley
import Control.Monad.Codensity
import Control.Monad.Reader
import GHC.Stack
import Notifications
import SetupHelpers
import Testlib.Prelude
import Testlib.ResourcePool

testMessageTimerChangeWithRemotes :: (HasCallStack) => App ()
testMessageTimerChangeWithRemotes = do
  [alice, bob] <- createAndConnectUsers [OwnDomain, OtherDomain]
  conv <- postConversation alice defProteus {qualifiedUsers = [bob]} >>= getJSON 201
  withWebSockets [alice, bob] $ \wss -> do
    void $ updateMessageTimer alice conv 1000 >>= getBody 200
    for_ wss $ \ws -> do
      notif <- awaitMatch isConvMsgTimerUpdateNotif ws
      notif %. "payload.0.qualified_conversation" `shouldMatch` objQidObject conv
      notif %. "payload.0.qualified_from" `shouldMatch` objQidObject alice

testMessageTimerChangeWithUnreachableRemotes :: (HasCallStack) => App ()
testMessageTimerChangeWithUnreachableRemotes = do
  resourcePool <- asks resourcePool
  alice <- randomUser OwnDomain def
  conv <- runCodensity (acquireResources 1 resourcePool) $ \[dynBackend] ->
    runCodensity (startDynamicBackend dynBackend mempty) $ \_ -> do
      bob <- randomUser dynBackend.berDomain def
      connectTwoUsers alice bob
      postConversation alice (defProteus {qualifiedUsers = [bob]}) >>= getJSON 201
  withWebSocket alice $ \ws -> do
    void $ updateMessageTimer alice conv 1000 >>= getBody 200
    notif <- awaitMatch isConvMsgTimerUpdateNotif ws
    notif %. "payload.0.qualified_conversation" `shouldMatch` objQidObject conv
    notif %. "payload.0.qualified_from" `shouldMatch` objQidObject alice
