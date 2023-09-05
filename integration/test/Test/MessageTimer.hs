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

module Test.MessageTimer where

import API.Brig
import API.Galley
import Data.Timeout
import GHC.Stack
import Notifications
import SetupHelpers
import Testlib.Prelude

testMessageTimerChangeWithRemotes :: HasCallStack => App ()
testMessageTimerChangeWithRemotes = do
  [alice, bob] <- createAndConnectUsers [OwnDomain, OtherDomain]
  conv <- postConversation alice defProteus {qualifiedUsers = [bob]} >>= getJSON 201
  client <- objId $ bindResponse (addClient bob def) $ getJSON 201
  let t = 1 # Second
      update :: Word64 = t #> MilliSecond
   in void $ updateMessageTimer alice conv update >>= getBody 200
  notif <- awaitNotification bob client noValue 2 isConvMsgTimerUpdateNotif
  notif %. "payload.0.qualified_conversation" `shouldMatch` objQidObject conv
  notif %. "payload.0.qualified_from" `shouldMatch` objQidObject alice
